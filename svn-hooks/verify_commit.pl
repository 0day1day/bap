#!/usr/bin/perl

use strict;
use Getopt::Long;
# apt-get install libsvn-hooks-perl or libsvn-svnlook-perl
use SVN::Core;
use SVN::Client;
use Term::ReadKey;
use Capture::Tiny qw/tee_merged/;  # requires package libcapture-tiny-perl 
$| = 1;

my $rep_url = 'file:///var/lib/svn/bap/trunk';
my $local_path = "/tmp/test-svn-hook";
my $true = 1;
my $curr_dir = `/bin/pwd`;
my $sendmail = '/usr/bin/sendmail';
my $svnlook = '/usr/bin/svnlook';
my ($revision, $repos);
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

    # Only grab the last 30 or so lines of output
    while (scalar(@lines) > 30) {
        shift(@lines);
    }

    if($result != 0) {
        print "Error! Last lines of ouput:\n";
        print (join("\n",@lines));
	print "\n";

	# go back to where we started
	chdir $curr_dir;
	# Send notification email
	open (MAIL, "|$sendmail -oi -t");
	print MAIL "From: svn\@kestrel.ece.cmu.edu\n";
#	print MAIL "To: bap-dev@lists.andrew.cmu.edu\n";
	print MAIL "To: swhitman\@andrew.cmu.edu\n";
	print MAIL "Subject: r$revision Build and Test failure!\n\n";
	print MAIL "Revision $revision failed to build or pass unit tests.  ";
	print MAIL "The last lines of output are provided below:\n";
	print MAIL @lines;
	print MAIL "\n";
	close MAIL;

	print "sendmail -oi -t:\n";
	print "From: svn\@kestrel.ece.cmu.edu\n";
	print "To: swhitman\@andrew.cmu.edu\n";
	print "Subject: r$revision Build and Test failure!\n\n";
	print "Revision $revision failed to build or pass unit tests.  ";
	print "The last lines of output are provided below:\n";
	print @lines;
	print "\n";

	# Clean up before exit
	print "Removing temporary repository at $local_path\n";
	check_system('rm', '-rf', $local_path);

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

# check out repository to /tmp
mkdir $local_path;
chdir $local_path;

my $ctx = new SVN::Client(auth => [SVN::Client::get_username_provider()]);
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
check_system("make test");

# go back to where we started
chdir $curr_dir;
# remove /tmp/trunk
print "Removing temporary repository at $local_path\n";
check_system('rm', '-rf', $local_path)
