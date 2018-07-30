use Text::CSV;
use strict;
use warnings;
use utf8;
use File::Copy;
binmode STDOUT, ':utf8';
use open qw/:std :utf8/;
 my $csv = Text::CSV->new ( { binary => 1 } )  # should set binary attribute.
                 or die "Cannot use CSV: ".Text::CSV->error_diag ();
my $filename = 'test_csv_1APR.csv';
open(my $fh, '<:encoding(UTF-8)', $filename)
or die "Could not open file '$filename' $!";
my $row = $csv->getline( $fh );
my $columns = $row;

# new becsles oldal
	open my $topfh, '<:encoding(UTF-8)', 'mandatumbecsles.md' or die "Can't open file $!";
	read $topfh, my $top_file_content, -s $topfh;
    my $new_top_file_content = $top_file_content;
while ($row = $csv->getline( $fh )) {
	chomp $row;
	my $mdfilename = "$row->[0].md";
	#print "$mdfilename\n";  - commented out
	open my $mdfh, '<:encoding(UTF-8)', $mdfilename or die "Can't open file $!";
	read $mdfh, my $file_content, -s $mdfh;
	#print "$file_content\n";
	my $new_file_content = $file_content =~ s/id="id_fidesz">xx%/id="id_fidesz">$row->[1]/gr;
	$new_file_content = $new_file_content =~ s/id="id_jobbik">xx%/id="id_jobbik">$row->[2]/gr;
	$new_file_content = $new_file_content =~ s/id="id_baloldal">xx%/id="id_baloldal">$row->[3]/gr;
	$new_file_content = $new_file_content =~ s/id="id_lmp">xx%/id="id_lmp">$row->[4]/gr;
	$new_file_content = $new_file_content =~ s/id="id_momentum">xx%/id="id_momentum">$row->[5]/gr;
	$new_file_content = $new_file_content =~ s/id="id_egyutt">xx%/id="id_egyutt">$row->[6]/gr;
	$new_file_content = $new_file_content =~ s/id="id_mkkp">xx%/id="id_mkkp">$row->[7]/gr;
	#$new_file_content = $new_file_content =~ s/id="esely">xx/id="esely">$row->[10]/gr;
	#$new_file_content = $new_file_content =~ s/id="esely2">xx/id="esely2">$row->[11]/gr;
	$new_file_content = $new_file_content =~ s/id="gyoztes">xx/id="gyoztes">$row->[21]/gr;
	$new_file_content = $new_file_content =~ s/id="masodik">xx/id="masodik">$row->[23]/gr;
	#$new_file_content = $new_file_content =~ s/id="profil">xx/id="profil">$row->[24]/gr;
	$new_file_content = $new_file_content =~ s/id="biztos_jelolt">xx/id="biztos_jelolt">$row->[24]/gr;
	$new_file_content = $new_file_content =~ s/id="id_fidesz2">xx%/id="id_fidesz2">$row->[8]/gr;
	$new_file_content = $new_file_content =~ s/id="id_jobbik2">xx%/id="id_jobbik2">$row->[9]/gr;
	$new_file_content = $new_file_content =~ s/id="id_baloldal2">xx%/id="id_baloldal2">$row->[10]/gr;
	$new_file_content = $new_file_content =~ s/id="id_lmp2">xx%/id="id_lmp2">$row->[12]/gr;
	$new_file_content = $new_file_content =~ s/id="id_momentum2">xx%/id="id_momentum2">$row->[13]/gr;
	$new_file_content = $new_file_content =~ s/id="id_egyutt2">xx%/id="id_egyutt2">$row->[14]/gr;
	$new_file_content = $new_file_content =~ s/id="id_mkkp2">xx%/id="id_mkkp2">$row->[15]/gr;
	
	#print $new_file_content;  - commented out
	
    open my $ofh, '>', "test_output/$mdfilename"; 
    print {$ofh} $new_file_content;
    close $ofh;
	$new_top_file_content = $new_top_file_content =~ s/id="gyoztes_$row->[0]">Győztes/id="gyoztes_$row->[0]">$row->[21]/gr;
		$new_top_file_content = $new_top_file_content =~ s/id="masodik_$row->[0]">Második/id="masodik_$row->[0]">$row->[22]/gr;
		$new_top_file_content = $new_top_file_content =~ s/id="kulonbseg_$row->[0]">Különbség/id="kulonbseg_$row->[0]">$row->[20]/gr;
		
};
open my $topofh, '>', "test_output/mandatumbecsles.md"; 
    print {$topofh} $new_top_file_content;
    close $topofh;

print "$columns\n";





