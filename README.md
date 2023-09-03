# Unhide

Hides a directory in an encrypted file.

Wraps a shell command, decrypts the directory dynamically before invoking that command, then encrypts and removes the directory again.
Decrypted directories are stored in a tmpfs or ramfs location to avoid disk storage.

Unhide uses `tar` and GnuPG under the hood. GnuPG is a trusted and versatile crypto framework that can be configured to encrypt for multiple users ("recipients").

## Usage

### Securing a directory for use by an application

`unhide -d directory -u userid -c app`

This creates a configuration file for the `app` command that contains the directory and the user key id for encryption.

This command interactively asks for parameters that are not given on the command line, and writes a configuration file that prepares `unhide` for wrapping the command and working transparently.

### Adding unhide to your PATH

export PATH=$HOME/.unhide/bin:$PATH

### Using the app command

Any invocation of the app command will run `unhide` under the cover from the directory `$HOME/.unhide/bin`.

## Configuratoin

### GPG key for encryption

We assume that there is just one local user accessing the encrypted directories. There are multiple ways to configure the encryption key:

* Set the "default-recipient" in `$HOME/.gnupg/gpg.conf` to your user for encrypting, to avoid the command line option or interactive query.
* Configure the key id in `$XDG_CONFIG_DIR/unhide/config.yaml`.
* Set the user id in the environment variable `UNHIDE_USER_ID`.

## Resources

- https://unix.stackexchange.com/questions/26364/how-can-i-create-a-tmpfs-as-a-regular-non-root-user#26366
