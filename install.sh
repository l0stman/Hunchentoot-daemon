#!/bin/sh -x

. ./htoot.conf

PW=${PW:-/usr/sbin/pw}
FILES="startup.lisp core.lisp htoot.conf htootd"

# Add the  user and group for Hunchentoot.
$PW groupshow $HT_GRP >/dev/null 2>&1
if [ $? -ne 0 ]; then
    $PW groupadd $HT_GRP
fi
$PW usershow $HT_USR  >/dev/null 2>&1
if [ $? -ne 0 ]; then
    $PW useradd \
        -n $HT_USR -g $HT_GRP -c Hunchentoot \
        -d $HT_HOME -m -M 700 \
        -s /usr/sbin/nologin
fi

# Install the different startup scripts.
su -m $HT_USR -c "cp -p $FILES $HT_HOME"
install -m 755 hunchentoot /etc/rc.d
