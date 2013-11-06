#! /bin/sh
# Generate thumbnails with ImageMagick
for file in *.jpg *.png; do
	convert ${file} -resize 50% thumb/${file};
done