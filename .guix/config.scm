;; This is an operating system configuration generated
;; by the graphical installer.

;;https://guix.gnu.org/manual/en/guix.html#index-Nix 
;; Steps for adding the nix package manager (maybe?)

(use-modules (gnu)
	     (nongnu packages linux)
	     (nongnu system linux-initrd))

(use-service-modules desktop networking ssh xorg)

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "America/Detroit")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "Voyager")
  (users (cons* (user-account
                  (name "Que")
                  (comment "Que")
                  (group "users")
                  (home-directory "/home/Que")
                  (supplementary-groups
                    '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))
  (packages
    (append
      (list (specification->package "nss-certs"))
      %base-packages))
  (services
    (append
      (list (service gnome-desktop-service-type)
            (service openssh-service-type)
            (set-xorg-configuration
              (xorg-configuration
                (keyboard-layout keyboard-layout))))
      %desktop-services))
  (bootloader
    (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (target "/boot/efi")
      (keyboard-layout keyboard-layout)))
  (swap-devices
    (list (uuid "eb89a5fe-1d2a-4322-ab22-25a6716a828d")))
  (file-systems
    (cons* (file-system
             (mount-point "/boot/efi")
             (device (uuid "7211-8566" 'fat32))
             (type "vfat"))
           (file-system
             (mount-point "/")
             (device
               (uuid "790bf06c-5226-4ff9-b04b-7d17340748dc"
                     'ext4))
             (type "ext4"))
           %base-file-systems)))
