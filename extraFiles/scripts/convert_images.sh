#/usr/bin/env bash
IFS=$'\n'

CONVERT="$(nix eval nixpkgs\#imagemagick.outPath | tr -d '"')/bin/convert"
convert_cmd () {
if  ! command -v convert &> /dev/null; then $CONVERT "$@"; else convert "$@"; fi
}



change_to_webp() {
 f="$1"
 file="${f%.*}"
 file_webp="${file}.webp"
 echo "Trying to convert to $file_webp"
 if convert_cmd "$f" "$file_webp"; then
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
 change_to_webp "$f" & 
done
	

for job in `jobs -p`
do
echo "Waiting for: $job"
    wait $job || let "FAIL+=1"
done

popd