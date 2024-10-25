{ pkgs, ... }:
{
  home.packages = [
    pkgs.papirus-icon-theme
    (pkgs.nerdfonts.override { fonts = [ "Hack" ]; })
  ];
  programs.konsole = {
    enable = true;
    profiles = {
      materus = {
        colorScheme = "Breeze";
        font.name = "Hack Nerd Font";
        extraConfig = {
          Scrolling = {
            HistoryMode = 2;
          };
        };
      };
    };
    extraConfig = {
      KonsoleWindow = {
        RememberWindowSize = "false";
      };
      MainWindow = {
        MenuBar = "Enabled";
      };

      "Desktop Entry" = {
        DefaultProfile = "materus.profile";
      };
    };
  };
  programs.plasma = {
    enable = true;
    overrideConfig = false;



    workspace = {
      lookAndFeel = "org.kde.breezedark.desktop";
      iconTheme = "Papirus-Dark";
    };

    shortcuts = {
      "kwin"."Grid View" = "Meta+Alt+Tab";
      "kwin"."Overview" = "Meta+Tab";
    };
    spectacle.shortcuts = {
      captureActiveWindow = "Meta+Print";
      captureCurrentMonitor = "Print";
      captureEntireDesktop = "Shift+Print";
      captureRectangularRegion = "Meta+S";
      launchWithoutCapturing = "Meta+Shift+S";
      launch = "Meta+Alt+S";
    };

    kwin = {
      effects = {
        wobblyWindows.enable = true;
      };
    };

    input = {
      keyboard = {
        options = [ "caps:none" ];
      };
    };
    kscreenlocker = {
      autoLock = false;
    };

    panels = [
      {
        location = "left";
        screen = 0;
        widgets = [
          {
            name = "org.kde.plasma.kickerdash";
            config = {
              General = {
                icon = "nix-snowflake-white";
                customButtonImage="nix-snowflake-white";
                alphaSort = true;
              };
            };

          }
          "org.kde.plasma.icontasks"
          "org.kde.plasma.marginsseparator"
          "org.kde.plasma.systemtray"
          {
            name = "org.kde.plasma.digitalclock";
            config = {
              Appearance = {
                showDate = "false";
              };
            };
          }
        ];

      }

    ];
    configFile = {
      "kwinrc"."Effect-overview"."BorderActivate" = 9;

      "kcminputrc"."Libinput/9610/46/SINOWEALTH Wired Gaming Mouse"."PointerAccelerationProfile" = 1;
      "kcminputrc"."Libinput/9610/47/SINOWEALTH 2.4G Wireless Receiver"."PointerAccelerationProfile" = 1;

      "spectaclerc"."ImageSave"."imageFilenameTemplate" = "<yyyy>-<MM>-<dd>.<hh>_<mm>_<ss>-<t>.materusPC.<#>";
      "spectaclerc"."VideoSave"."videoFilenameTemplate" = "<yyyy>-<MM>-<dd>.<hh>_<mm>_<ss>-<t>.materusPC.<#>";
      "spectaclerc"."ImageSave"."preferredImageFormat" = "WEBP";
      "spectaclerc"."ImageSave"."translatedScreenshotsFolder" = "Zrzuty ekranu";
      "spectaclerc"."VideoSave"."translatedScreencastsFolder" = "Nagranie ekranu";

      "dolphinrc"."General"."RememberOpenedTabs" = false;

      "kwalletrc"."Wallet"."Enabled" = false;

    };
    dataFile = {
      "dolphin/view_properties/global/.directory"."Settings"."HiddenFilesShown" = true;
    };

  };
}
