#!/bin/sh

: ${F:=${2:-"$(dirname $0)/tilix.conf"}}
echo "Using config: $F"

case "$1" in 
  save|dump) 
	set -x
	dconf dump /com/gexperts/Tilix/ > $F
	set +x
  ;;
  load)
	[ -r "$F" ] || ( echo "$F not found"; exit 1 )
	set -x
	dconf load /com/gexperts/Tilix/ < $F
	set +x

  ;;
  *)
	echo "Usage: $0 save|load"
	exit 1
  ;;
esac

