(specifications->manifest
      '("emacs-native-comp"	;Emacs text editor locally compilled
	"vim"		;Vim text editor
	"git"		;Git version control
	"lxde"		;LXDE Display manager
	"firefox"	;Firefox nightly, locally compiled
	"font-overpass" ;Overpass font theme - https://overpassfont.org/
	"nix"		;NIX package manager
	"flatpak"	;Flatpak package manager
	"emacs-guix"	;Guix pkm interface for emacs
	"unclutter"	;Hide cursor when mouse is inactive
	))

;;after updating run "guix package --manifest=.dotfiles/guix/manifest.scm"
