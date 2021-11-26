(specifications->manifest
      '("emacs-native-comp"	;Emacs text editor locally compilled
	"vim"		;Vim text editor
	"git"		;Git version control
	"lxde"		;LXDE Display manager
	"firefox"	;Firefox nightly, locally compiled
	"font-overpass" ;Overpass font theme - https://overpassfont.org/
	))

;;after updating run "guix package --manifest=.dotfiles/guix/manifest.scm"
