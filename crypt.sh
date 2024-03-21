#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd $SCRIPT_DIR

if [ $# = 0 ] || ( [ $# = 1 ] && [ $1 = "help" ] ); then
echo "Use \"unlock\" to unlock with pgp"
echo "Use \"unlock\" /path/to/key to unlock with symmetric key"
echo "Use \"lock\": to lock repository"
exit
fi
check_git () {
if [ "$(git status --porcelain)" ]; then
echo "Working directory not clean."
echo "Please commit your changes or 'git stash' them before running this script"
exit 1
fi
}

create_decrypt () {
    touch decrypted
    git add --intent-to-add decrypted
    git update-index --assume-unchanged decrypted
}
delete_decrypt () {
    rm decrypted
    git rm decrypted
}


if [ $# = 1 ]; then

    if [ $1 = "unlock" ]; then
        check_git
        git-crypt unlock
        create_decrypt
        exit
    fi

    if [ $1 = "lock" ]; then
        check_git
        delete_decrypt
        git-crypt lock
        exit
    fi

    if [ $1 = "create_decrypt" ]; then
        create_decrypt
        exit
    fi

    if [ $1 = "delete_decrypt" ]; then
        delete_decrypt
        exit
    fi

fi

if [ $# = 2 ] && [ $1 = "unlock" ]; then
    git-crypt unlock $2
    create_decrypt
    exit
fi