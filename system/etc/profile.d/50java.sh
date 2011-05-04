#!/bin/bash

if [ -d "/etc/java.d" ]; then
	set -a
	while read f; do
		if [ -f "${f}" ]; then
			#echo  "java.d: sourcing ${f} ..."
			 . "${f}"
		fi
	done <<- EOF
		`/usr/bin/find /etc/java.d -iname '*.conf' -type f | sort`
	EOF
	set +a
fi


