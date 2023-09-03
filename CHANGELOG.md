# Revision history for unhide

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## In progress

* [ ] detect and act upon the directory status

## Backlog

* [ ] check directory size before encrypting, and warn if above a limit
   * since the decrypted data is going to be stored in memory

* [ ] refuse to hide if the directory is a symlink
* [ ] refuse to decrypt if the directory is not a symlink

* gopass support
   * [ ] find store directory and us it as default
