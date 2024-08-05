# .bashrc

#
# Standard stuff whcih came with the AWS EC2 Linux version
#

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

#
# In GeoSoft we prefer tcsh
#
tcsh -i
