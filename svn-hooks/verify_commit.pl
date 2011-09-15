#!/usr/bin/perl

use strict;
use Getopt::Long;
# apt-get install libsvn-hooks-perl or libsvn-svnlook-perl
use SVN::Core;
use SVN::Client;
use Term::ReadKey;
use Capture::Tiny qw/tee_merged/;  # requires package libcapture-tiny-perl 
$| = 1;

my $rep_url = 'https://kestrel.ece.cmu.edu/svn/bap/trunk';
my $local_path = "/tmp/test-svn-hook";
my $true = 1;
my $curr_dir = `pwd`;
my $svn_look = '/usr/bin/svnlook';
my ($revision, $repos, $svnlook);
my $getpin = 0;
my $merged;

GetOptions(
    'revision|r=s'    => \$revision,
    'repos=s'         => \$repos,
    'svnlook=s'       => \$svnlook,
    'getpin'          => \$getpin,
    );
#
# Keep track of the last 10 lines to print out in case of error; Exit on error
#
sub check_system {
    my @cmd = @_;
    my @lines;
    my $result, $merged;
    $merged = tee_merged {
        $result = system(@cmd);
    };
    @lines = split(/\n/,$merged);
    while (scalar(@lines) > 10) {
        shift(@lines);
    }
    if($result != 0) {
        print "Error! Last lines of ouput:\n";
        print (join("\n",@lines));
        print "\nExiting...\n";
        exit(-1);
    }
}

#
# Simple prompt to get svn authentication for checkout; for testing.
#
sub simple_prompt {
    my $cred = shift;
    my $realm = shift;
    my $default_username = shift;
    my $may_save = shift;
    my $pool = shift;

    print "Enter authentication info for realm: $realm\n";
    print "Username: ";
    my $username = <>;
    chomp($username);
    $cred->username($username);

    print "Type your password:";
    ReadMode('noecho'); # don't echo
    chomp(my $password = <STDIN>);
    ReadMode(0);        # back to normal
    $cred->password($password);
    print "\n";
}

# Did user provide opt-out message?
exit (0) if(`$svnlook log -r $revision $repos` =~ /unittest-opt-out/);

# Make sure this revision touches trunk
exit (0) if(`$svnlook changed -r $revision $repos` !~ /[U|A|D]\s+trunk.*$/);

# check out repository to /tmp
mkdir $local_path;
chdir $local_path;

my $ctx = new SVN::Client(
    auth => [SVN::Client::get_simple_provider(),
             SVN::Client::get_simple_prompt_provider(\&simple_prompt,2),
             SVN::Client::get_username_provider()]
    );

print "Checking out repository $rep_url\n";

$ctx->checkout($rep_url, $local_path, $revision, $true);

print "Finished checking out repository $rep_url\n";

# configure
print "Configuring\n";
check_system("./autogen.sh");
check_system("./configure", "CFLAGS=-DAMD64");

# getpin?
if($getpin) {
    print "Getting pin\n";
    chdir "pintraces";
    check_system("./getpin.sh");
    chdir "../";
}

# make test
print "Making test\n";
check_system("make");

# go back to where we started
chdir $curr_dir;
# remove /tmp/trunk
print "Removing temporary repository at $local_path\n";
check_system('rm', '-rf', $local_path)
