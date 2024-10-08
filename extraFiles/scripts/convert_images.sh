#/usr/bin/env bash
IFS=$'\n'
nix build nixpkgs\#imagemagick --no-link
CONVERT="$(nix eval nixpkgs\#imagemagick.outPath | tr -d '"')/bin/magick"
convert_cmd () {
if  ! command -v magick &> /dev/null; then $CONVERT "$@"; else convert "$@"; fi
}


function max16 {
   while [ `jobs | wc -l` -ge 16 ]
   do
      sleep 2
   done
}

change_to_webp() {
 f="$1"
 file="${f%.*}"
 file_webp="${file}.webp"
 echo "Trying to convert to $file_webp"
 if convert_cmd "$f" -define webp:thread-level=1 -define webp:method=6 -quality 99 "$file_webp"; then
  if touch -r "$f" "$file_webp"; then
   rm "$f"
   echo "Finished converting $f"
  else
   echo "Failed to set old date to new file"
   exit 1
  fi
 else
  echo "Failed to convert $f"
  exit 1;
 fi
}

pushd $XDG_PICTURES_DIR
for f in `find "." \( -name "*.png" -type f -o -name "*.jpg" -type f -o -name "*.jpeg" -type f -o -name "*.avif" -type f \) \
        -a -not \( -path "./Inne/Special/*" -o -path "./Inne/Emojis/*" -o -path "./Inne/MCSkins/*" -o -path "./Avatar/*" -o -path "./Inne/GIF/*" \)`; 
do
 max16; change_to_webp "$f" & 
done
	

for job in `jobs -p`
do
echo "Waiting for: $job"
    wait $job || let "FAIL+=1"
done

popd