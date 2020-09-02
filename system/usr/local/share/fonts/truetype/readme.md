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

* [The 10 best fonts for programming: A guide](https://developer-tech.com/news/2018/may/16/10-best-fonts-programming/)

INSTALL
-------

```bash

firefox https://tinyurl.com/h37-z-fonts

mkdir -p  /usr/local/share/fonts/truetype
cd        /usr/local/share/fonts/truetype

ls -l    ttf-iosevka.tar.xz
tar xvJf ttf-iosevka.tar.xz
rm       ttf-iosevka.tar.xz


sudo fc-cache -fv

sudo fc-list | grep -i iosevka | cut -d' ' -f2- | sort | less -SRX


```

