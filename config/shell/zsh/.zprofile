__HOME_ZPROFILE_SOURCED=1
[[ -f "$ZSH_DATA_DIR/nix_profile.sh" ]] && source "$ZSH_DATA_DIR/nix_profile.sh" 
[[ -f "/etc/profile" ]] && emulate sh -c "source /etc/profile"
[[ -f "~/.profile" ]] && emulate sh -c "source ~/.profile" 


