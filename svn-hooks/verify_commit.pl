#!/usr/bin/perl

#
# NOTE: This file is in svn for tracking purposes.  The actual file which is 
# run is on kestrel at /var/lib/svn/bap/hooks/verify_commit.pl  Changes to this
# file must also be made to /var/lib/svn/bap/hooks/verify_commit.pl.
#

use strict;
use Getopt::Long;
use SVN::Core;
use SVN::Client;
use Term::ReadKey;
use Capture::Tiny qw/tee_merged/;  # requires package libcapture-tiny-perl 
use Fcntl qw(:flock);

$| = 1;

my $rep_url = 'file:///var/lib/svn/bap/trunk';
my $local_path = "/tmp/test-svn-hook";
my $true = 1;
my $mail = '/usr/bin/mail';
my $svnlook = '/usr/bin/svnlook';
my ($revision, $repos);
my $getpin;
my $merged;
my $subj;
#my $to = 'swhitman@andrew.cmu.edu';
my $to = 'bap-dev@lists.andrew.cmu.edu';


GetOptions(
    'revision|r=s'    => \$revision,
    'repos=s'         => \$repos,
    'svnlook=s'       => \$svnlook,
    'getpin'          => \$getpin,
    );

#
# Keep track of the last 10 lines to print out in case of error; Clean up and 
# Exit on error.
#
sub check_system {
    my @cmd = @_;
    my @lines;
    my $result, $merged;
    my $msg;

    $merged = tee_merged {
        $result = system(@cmd);
    };

    @lines = split(/\n/,$merged);

    # Only grab the last 30 or so lines of output
    while (scalar(@lines) > 30) {
        shift(@lines);
    }

    if($result != 0) {
        print "Error executing cmd:\n@cmd\nLast lines of ouput (if any):\n";
	$msg = (join("\n",@lines)) . "\n";
	print $msg;

	# leave $local_path before removing it
	chdir '/';

	# Clean up before exit
	print "Removing temporary repository at $local_path\n";
	check_system('/bin/rm', '-rf', $local_path);

	# Send notification email
	$subj = "FAILURE: r$revision - Build and Test failure!";
	open (MAIL, "|-",$mail, "-s",$subj,$to) or 
	    die "Can't open pipe to mail: $!\n";

	print MAIL "Revision $revision failed to build or pass unit tests.  ";
	print MAIL "The last lines of output are provided below:\n";
	print MAIL "\n<...snip...>\n";
	print MAIL $msg;
	close MAIL;

        print "Exiting...\n";
        exit($result);
    }
}


die "Incorrect options!\n" 
    unless (defined $revision && defined $repos && defined $svnlook);

# Did user provide opt-out message?
exit(0) if(`$svnlook log -r $revision $repos` =~ /unittest-opt-out/);

# Make sure this revision touches trunk
exit(0) if(`$svnlook changed -r $revision $repos` !~ /[U|A|D]\s+trunk\/.*$/);

# Make sure at most one process runs at a time
print "Acquiring exclusive lock\n";
flock(DATA, LOCK_EX) or die "Couldn't acquire lock...Exiting!\n";

print "Verifying commit revision $revision to repo $repos\n";

# check out repository to /tmp
mkdir $local_path;
chdir $local_path;

print "Checking out repository $rep_url\n";

my $ctx = new SVN::Client(auth => [SVN::Client::get_username_provider()]);
$ctx->checkout($rep_url, $local_path, $revision, $true);

print "Finished checking out repository $rep_url\n";

# Set path variable so autogen will work
$ENV{'PATH'} = '/bin:/usr/bin';
#$ENV{'PIN_HOME'} = '/home/swhitman/pin/';

# configure
print "Configuring\n";
check_system("./autogen.sh");
check_system("./configure");

# getpin?
if(defined $getpin) {
    print "Getting pin\n";
    chdir "pintraces";
    check_system("./getpin.sh");
    chdir "../";
}


# make test
print "Making test\n";
check_system("make -j test");


# leave $local_path before removing it
chdir '/';
# remove /tmp/trunk
print "Removing temporary repository at $local_path\n";
check_system('/bin/rm', '-rf', $local_path)

# This section is used for locking and must be the last line of this file.
# DO NOT (RE)MOVE!
__DATA__
