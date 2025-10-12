-- Pull in the wezterm API
local wezterm = require 'wezterm'


local config = wezterm.config_builder()

-- This is where you actually apply your config choices
--


config.wsl_domains = {
  {
    -- The name of this specific domain.  Must be unique amonst all types
    -- of domain in the configuration file.
    name = 'WSL:Ubuntu',

    -- The name of the distribution.  This identifies the WSL distribution.
    -- It must match a valid distribution from your `wsl -l -v` output in
    -- order for the domain to be useful.
    distribution = 'Ubuntu',
  },
}
--config.default_domain = 'WSL:Ubuntu'



-- You can specify some parameters to influence the font selection;
-- for example, this selects a Bold, Italic font variant.
-- config.font = wezterm.font('JetBrains Mono', { weight = 'Bold', italic = true })
--


config.font = wezterm.font_with_fallback {
  'Fira Code',
  'DengXian',
  'Consolas',
}



-- For example, changing the color scheme:
config.color_scheme = 'Argonaut'

-- and finally, return the configuration to wezterm
return config

