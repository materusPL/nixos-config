(message "Config loading from package")
(setq-default materus/nix-packages t)
(load-relative "materus/init")
(provide 'materus-config)