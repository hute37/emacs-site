#!/bin/bash


##
# git: check if pull needed
#
# http://stackoverflow.com/questions/3258243/git-check-if-pull-needed
# 


git remote update

git status -uno
git status -s -u no

git show-branch *master


git fetch -v --dry-run
git pull --dry-run | grep -q -v 'Already up-to-date.' && changed=1

git ls-remote origin -h refs/heads/master

git rev-list HEAD...origin/master --count


git fetch origin

# See if there are any incoming changes
git log HEAD..origin/master --oneline

git fetch; git log HEAD.. --oneline

git rev-list HEAD...origin/master --count


git diff remotes/origin/HEAD

#
# gitcheck
# https://github.com/badele/gitcheck

gitcheck.py
gitcheck.py -h



