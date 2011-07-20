#!/usr/bin/perl

my ($mainAddr,$retAddr,$labelStr);

my $file = "./C/test";
my $ilFile = "$file.il";
my $outFile = "$file.il.halted";

# Find the main start and "end" address.  "end" address is really address of 
# first ret instruction after finding "<main>" in objdump.
open(OBJDUMP, "objdump -D $file |") || die "Could not objdump: $!\n";
foreach $line (<OBJDUMP>) {
    if (defined $mainAddr && 
        $line =~ /([0-9|a-e]+):\s+\S+\s+ret/) {
        print "Ret line is:\n$line";
        print "Ret Addr = $1\n";
        $retAddr = $1;
        last;
    }

    if ($line =~ /^([0-9|a-e]+) <main>/) {
        print "Main Addr = $1\n";
        $mainAddr = $1;
    }
}
close(OBJDUMP);


$labelStr = "label pc_0x$retAddr";

# Print out a new il file with "halt true" after the ret found in <main>
open(IL, "<$ilFile") || die "Could not open file $ilFile: $!\n";
open(ILOUT, ">$outFile") || die "Could not open file $outFile: $!\n";
foreach $line (<IL>) {
    print ILOUT $line;
    if($line =~ /$labelStr/) {
        print ILOUT "halt true\n";
        print "Il line is $line";
    }
}
close(IL);
close(ILOUT);

exit 0;
# 080482d7 <main>:
#  80482d7:       55                      push   %ebp
#  80482d8:       89 e5                   mov    %esp,%ebp
#  80482da:       83 ec 04                sub    $0x4,%esp
#  80482dd:       c7 04 24 2a 00 00 00    movl   $0x2a,(%esp)
#  80482e4:       e8 d7 ff ff ff          call   80482c0 <g>
#  80482e9:       c9                      leave  
#  80482ea:       c3                      ret    
#  80482eb:       90                      nop
#  80482ec:       90                      nop
#  80482ed:       90                      nop
#  80482ee:       90                      nop
#  80482ef:       90                      nop
