#!/bin/bash

# convert DjVu -> PDF
# usage:  djvu2pdf.sh [-c|-b] <file.djvu>

mode='color'
quality=80

aparse() {
  while [ $# != 0 ] ; do
    case "$1" in
    -q|--quality)
      quality=${2}
      shift
      ;;
    -b|--black)
      mode='black'
      ;;
  esac
  shift
done
}

aparse "$@"


i="$1"
echo "------------ converting $i to PDF ----------------";
o=${2:-$(basename $i .djvu).pdf}

if [ -f  "$o" ]; then 
  echo "file $o exists, override [Y/n]?"
  read ans
  case "$ans" in 
   n|N) exit 1;;
  esac
fi


echo "[ writing output to $o ] "

cmd="ddjvu -format=pdf -quality=$quality -mode=$mode -verbose $i $o "

echo "[ executing $cmd ] "

$cmd
