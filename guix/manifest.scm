(specifications->manifest
      '("emacs-native-comp"	;Emacs text editor locally compilled
	"vim"			;Vim text editor
	"git"			;Git version control
	"lxde"			;LXDE Display manager
	"firefox"		;Firefox nightly, locally compiled
	"font-overpass" 	;Overpass font theme - https://overpassfont.org/
	"font-jetbrains-mono" 	;JetBrains font theme - https://www.jetbrains.com/
	"fontconfig"		;After installing this and target fonts run "$fc-cache -rv"
	"nix"			;NIX package manager
	"hunspell"		;Hunspell spellchecker backend
	"unclutter"		;Hide cursor when mouse is inactive
	"gcc-toolchain" 	;C/C++ compiler tools.
	"clang"         	;C/C++ compiler front end.
	"stow"			;Symlink manager
	))

;;after updating run "guix package --manifest=.dotfiles/guix/manifest.scm"
