# ========================================================================
# Makefile for Pandoc Markdown-to-PDF Conversion
# ========================================================================
# Purpose: Automate PDF generation from Markdown using Pandoc + LuaLaTeX
# Engine: LuaLaTeX (requires fontspec, unicode-math support)
# Platform: Ubuntu 24.04 with TeX Live and Pandoc
# ========================================================================


# ------------------------------------------------------------------------
# Module
# ------------------------------------------------------------------------

STYLES_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
STYLES_DIR := $(patsubst %/,%,$(dir $(STYLES_PATH)))
ROOT_DIR := $(shell (cd ${STYLES_DIR} && git rev-parse --show-toplevel))


# ------------------------------------------------------------------------
# Targets
# ------------------------------------------------------------------------

SOURCES ?= \
	$(shell grep -l -e '^doctype:' *.md)

OBJECTS := $(SOURCES:.md=.pdf)
TEXOUTS := $(SOURCES:.md=.tex)



# ------------------------------------------------------------------------
# Base
# ------------------------------------------------------------------------

MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
MAKE_DIR := $(patsubst %/,%,$(dir $(MAKEFILE_PATH)))
ROOT_DIR := $(shell (cd ${MAKE_DIR} && git rev-parse --show-toplevel))
WORK_DIR := $(patsubst ${HOME}/%,./%,${ROOT_DIR})
CONF_DIR := $(patsubst ${ROOT_DIR}/%,./%,${MAKE_DIR})
MAKEFILE_FOLDER := $(notdir ${MAKE_DIR})



# ------------------------------------------------------------------------
# Styles
# ------------------------------------------------------------------------

YML_DEFAULTS := $(STYLES_DIR)/md-report.yaml

TEX_PREAMBLE := $(STYLES_DIR)/md-report.tex
LUA_FILTER   := $(STYLES_DIR)/md-report.lua

TEX_TEMPLATE := $(STYLES_DIR)/md-template.tex


LUA_PATH := $(STYLES_DIR)/?.lua;${LUA_PATH};;


# ------------------------------------------------------------------------
# Format
# ------------------------------------------------------------------------

#IN_FORMAT  := markdown
IN_FORMAT  := gfm+yaml_metadata_block+raw_attribute
OUT_FORMAT := pdf
TEX_FORMAT := latex

# Define the AWK command in a variable for readability.
# IMPORTANT: All '$' signs for awk fields must be doubled to '$$' for Make.
GET_DOCTYPE = awk ' \
	/^---$$/ { if (NR==1) { fm=1; next } if (fm) exit } \
	fm && /^[[:space:]]*doctype:/ { \
		sub(/^[[:space:]]*doctype:[[:space:]]*/, ""); \
		sub(/[[:space:]]*$$/, ""); \
		val=$$0 \
	} \
	END { if (val != "") print val } \
'

# ------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------

#        --template=$(TEX_TEMPLATE) \
#        --defaults=$(YML_DEFAULTS) \
#        --include-in-header=$(TEX_PREAMBLE) \
#        --lua-filter=$(LUA_FILTER) \
#        --table-of-contents \
#        --toc-depth=3 \

PANDOC      := pandoc
PDF_ENGINE  := lualatex



PANDOC_FLAGS = \
	${PANDOC_OPTS} \
        --standalone

PANDOC_DEBUG = \
        ${PANDOC_OPTS} \
        --to=$(TEX_FORMAT) \
        --verbose \
        --standalone


EX_PANDOC_FLAGS = \
	${PANDOC_OPTS} \
	--pdf-engine=$(PDF_ENGINE) \
	--from=$(IN_FORMAT) \
        --to=$(OUT_FORMAT) \
        --defaults=$(YML_DEFAULTS) \
        --lua-filter=$(LUA_FILTER) \
        --include-in-header=$(TEX_PREAMBLE) \
        --standalone

EX_PANDOC_DEBUG = \
	${PANDOC_OPTS} \
	--pdf-engine=$(PDF_ENGINE) \
	--from=$(IN_FORMAT) \
        --to=$(TEX_FORMAT) \
        --defaults=$(YML_DEFAULTS) \
        --lua-filter=$(LUA_FILTER) \
        --include-in-header=$(TEX_PREAMBLE) \
	--verbose \
        --standalone


CLEAN_OPTS := "-I" # rm options, ask once



#{{{ [ RULES.* ] /////////////////////////////////////////////////////////////////

.SUFFIXES:.pdf .tex .md

# This pattern rule applies to all Markdown files in the directory.
# Dependencies: the source .md file + the shared preamble

%.pdf: %.md
	$(eval TEMP_DIR := $(shell mktemp -d /tmp/pandoc-XXXXXX))
	$(eval DOCS_DIR := $(patsubst ${ROOT_DIR}/%,%,$(shell pwd)))
	doctype=$$($(GET_DOCTYPE) $<) ; \
	echo "### Building $@ from $< with $$doctype..." ; \
	LUA_PATH="$(LUA_PATH)" \
	$(PANDOC) $(PANDOC_FLAGS) \
		--defaults="$(STYLES_DIR)/$$doctype.yaml"  \
	        --metadata=tempdir=$(TEMP_DIR) \
	        --metadata=docsdir=$(DOCS_DIR) \
	        --metadata=docname="$@" \
	        --pdf-engine-opt=-outdir=$(TEMP_DIR) \
		"$<" -o "$@"

%.tex: %.md
	$(eval TEMP_DIR := $(shell mktemp -d /tmp/pandoc-XXXXXX))
	$(eval DOCS_DIR := $(patsubst ${ROOT_DIR}/%,%,$(shell pwd)))
	doctype=$$($(GET_DOCTYPE) $<) ; \
	echo "### Building $@ from $< with $$doctype..." ; \
	LUA_PATH="$(LUA_PATH)" \
	$(PANDOC) $(PANDOC_DEBUG) --log=$(@:.tex=.log) \
		--defaults="$(STYLES_DIR)/$$doctype.yaml" \
	        --metadata=tempdir=$(TEMP_DIR) \
	        --metadata=docsdir=$(DOCS_DIR) \
	        --metadata=docname="$@" \
	        --pdf-engine-opt=-outdir=$(TEMP_DIR) \
		"$<" -o "$@"

#}}} \\\

#{{{ [ BUILD.* ] /////////////////////////////////////////////////////////////////


# ---(phony)------------------------------------------------

.PHONY: all

# ---(all)------------------------------------------------
# Build all example PDFs

all: $(OBJECTS)

# ---(debug)------------------------------------------------
# Build all tex files

debug: $(TEXOUTS)


# ------------------------------------------------------------------------
# Specific Targets for Example Files
# ------------------------------------------------------------------------
# These targets explicitly define dependencies for specific documents.
# Useful if you have additional per-document dependencies later.

# a.pdf: a.md
# b.pdf: b.md
# c.pdf: c.md


#}}} \\\




#{{{ [ UTILS.* ] /////////////////////////////////////////////////////////////////

# ---(phony)------------------------------------------------

.PHONY: list ls clean clear

# ---(template)------------------------------------------------
# List all source and targets
list:
	@echo "(=) ///////////////////////////////////////////////////////////"
	@echo "(=) /// (GLFM) Markdown -> (pandoc=>LuaLaTeX) -> PDF Pipeline"
	@echo "(=) ///////////////////////////////////////////////////////////"
	@echo "(=) --- All Markdown Sources:"
	@echo  $(SOURCES)
	@echo "(=)"
	ls -l $(SOURCES) || true
	@echo "(=) --- All Generated PDF Documents:"
	@echo  $(OBJECTS)
	@echo "(=)"
	ls -l $(OBJECTS)  || true
	@echo "(=) ////////////////////////////////////////////////////////"

ls: list # command alias

# ---(clean)------------------------------------------------
# Remove generated PDFs
clean:
	@echo "(!) Removing all generated .pdf files ..."
	@ls -l $(OBJECTS) || true
	rm -v $(CLEAN_OPTS) $(OBJECTS) || true
	@ls -l $(OBJECTS) || true

# ---(clear)------------------------------------------------
# Remove generated tex files
clear:
	@echo "(!) Removing all generated .tex files ..."
	@ls -l $(TEXOUTS) || true
	rm -v $(CLEAN_OPTS) $(TEXOUTS) || true
	@ls -l $(TEXOUTS) || true

#}}} \\\


#{{{ [ UTILS.* ] /////////////////////////////////////////////////////////////////

# ---(phony)------------------------------------------------

.PHONY: watch template

# ---(template)------------------------------------------------
# export pandoc default template

template:
	@echo "(*) Watching for changes in .md files ..."
	pandoc -D latex > $(TEMPLATE)
	@ls -l $(TEMPLATE)

# ---(watch)------------------------------------------------
# Watch for changes (requires inotify-tools on Ubuntu)

watch:
	@echo "(*) Watching for changes in .md files ..."
	@while inotifywait -q -e close_write *.md $(PREAMBLE); do \
	make all; \
	done

#}}} \\\



#{{{ [ HELP.* ] /////////////////////////////////////////////////////////////////

# ---(phony)------------------------------------------------

.PHONY: help print-%

.DEFAULT_GOAL := help

# ---(print)------------------------------------------------
# Display the value.
# ex. $ make print-REPORT_SOURCE_DIR
# ex. $ make print-IMAGE_REVISION

print-%:
	@echo $* = $($*)

# ---(help)------------------------------------------------

help:
	@echo "(=) ///////////////////////////////////////////////////////////"
	@echo "(=) /// (GLFM) Markdown -> (pandoc=>LuaLaTeX) -> PDF Pipeline"
	@echo "(=) ///////////////////////////////////////////////////////////"
	@echo ""
	@echo "Available targets:"
	@echo "  all       - Build all PDFs (a.pdf, b.pdf, c.pdf)"
	@echo "  list      - List MD (with '^documentclass') and PDFs"
	@echo "  clean     - Remove generated PDFs"
	@echo "  watch     - Watch for changes and rebuild automatically"
	@echo "  %.pdf     - Build specific PDF from corresponding .md file"
	@echo "  help      - Usage documentation"
	@echo ""
	@echo "Usage examples:"
	@echo "  make all         # Build all env 'SOURCES' PDFs"
	@echo "  make list        # List souce MD files and target PDF files"
	@echo "  make notes.pdf   # Build any notes.md → notes.pdf by generic rule"
	@echo "  make clean       # Remove all generated PDFs"
	@echo ""
	@echo "Command Options:"
	@echo "  CLEAN_OPTS=--force make clean # No Prompt (batch) clean"
	@echo ""
	@echo "Debug Mode:"
	@echo "  PANDOC_OPTS='--verbose --log=pandoc.log' make all  # to add extra pandoc options"
	@echo ""
	@echo "NOTE:"
	@echo "  warning: filenames with spaces are NOT supported!"
	@echo "  rename markdown files with spaces ' ' replaced by hyphens '-' "
	@echo ""
#}}} \\\
# vim: set foldmethod=marker :
