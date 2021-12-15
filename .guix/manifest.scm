(specifications->manifest
      '("emacs-native-comp"	;Emacs text editor locally compilled
	"vim"			;Vim text editor
	"git"			;Git version control
	"firefox"		;Firefox nightly, locally compiled
	;"nix"			;Nix packagemanager
	"font-overpass" 	;Overpass font theme - https://overpassfont.org/
	"font-jetbrains-mono" 	;JetBrains font theme - https://www.jetbrains.com/
	"fontconfig"		;After installing this and target fonts run "$fc-cache -rv"
	"hunspell"		;Hunspell spellchecker backend
	"hunspell-dict-en-us"   ;US English dictionary for hunspell
	"unclutter"		;Hide cursor when mouse is inactive
	"gcc-toolchain" 	;C/C++ compiler tools.
	"clang"         	;C/C++ compiler front end.
	"stow"			;Symlink manager
	"curl"			;Terminal URL download tool
	))

;;after updating run "guix package --manifest=.dotfiles/guix/manifest.scm"
;
;; Nix PKM installed using bash <(curl -L https://nixos.org/nix/install) --no-deamon
