--
title: DsLab Site Setup
subtitle: Wordpress DEMS/datalab web site
author: --
date: 2019-06-18
---

IOSEVKA FONT
============


REFERENCES
----------

* [Iosevka](https://typeof.net/Iosevka/)
* [Iosevka - GitHub](https://github.com/be5invis/Iosevka)
* [Iosevka - Wikipedia](https://en.wikipedia.org/wiki/Iosevka)

INSTALL
-------

```bash

wget --no-check-certificate 'https://docs.google.com/uc?export=download&id=1XhaddF8MjU5yYCGInW3kp5sltu_IBZWr -O ttf-iosevka.tar.xz

mkdir -p  /usr/local/share/fonts/truetype
cd        /usr/local/share/fonts/truetype

ls -l    ttf-iosevka.tar.xz
tar xvJf ttf-iosevka.tar.xz
rm       ttf-iosevka.tar.xz


sudo fc-cache -fv

sudo fc-list | grep -i iosevka | cut -d' ' -f2- | sort | less -SRX


```

