#!/bin/bash

LST=`cat links.txt`;
x=1;
for i in $LST; do
    echo "Downloading $i...";
    wget $i -O $x.html;
    echo "Converting $x...";
    wkhtmltopdf $x.html $x.pdf;
    let x=x+1;
done
