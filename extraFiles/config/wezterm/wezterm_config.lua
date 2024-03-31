local wezterm_config = {};

function materus_wezterm_config()
    local wezterm = require 'wezterm';
    local cfg = wezterm.config_builder();
    cfg.hide_tab_bar_if_only_one_tab = true;
    cfg.enable_scroll_bar = true;
    return cfg;
end
