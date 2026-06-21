__HOME_ZPROFILE_SOURCED=1
[[ -f "/etc/profile" ]] && emulate sh -c "source /etc/profile"

if [[ -d ~/.nix-profile/etc/profile.d ]]; then
    for file in ~/.nix-profile/etc/profile.d/*; do
        [ -f "$file" ] && [ -r "$file" ] && emulate sh -c "source \"$file\""
    done
fi

[[ -f "~/.profile" ]] && emulate sh -c "source ~/.profile" 


