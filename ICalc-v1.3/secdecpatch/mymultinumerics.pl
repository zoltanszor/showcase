use Getopt::Long;
GetOptions("parameter=s" => \$multiparam, "dirwork=s"=>\$workingdir);
unless ($multiparam) {
  $multiparam = "multiparam.input";
}
$mparam=$multiparam;
if($workingdir){
 $workingdir=~s/\/$//;
 $mparam="$workingdir/$multiparam";
 $absdir=1;
}
unless($workingdir){$workingdir=`pwd`;chomp $workingdir}
my %hash_var=readmultiparams($mparam);
$paramfile=$hash_var{"paramfile"};
$linenumbers=$hash_var{"lines"};
$flag=$ARGV[0];

if($hash_var{"values"}){
 @values=split(/;/,$hash_var{"values"});
 $numparams=split(/,/,$values[0]);
 @vals=();
 for($i=0;$i<$linenumbers;$i++){
  @tvals=split(/,/,$values[$i]);
  $nvals[$i]=@tvals;
  push(@vals,[@tvals]);
 }
 if($flag==2){
  print "removing intermediate parameter files...\n";
 }elsif($flag==1){
  print "$linenumbers points to collect results for...\n";
 }else{
  print "$linenumbers points to calculate\n";
  system("perl -pi -e 's/^editor=.+/editor=none/g' $workingdir/$paramfile");
 }
 for($k=0;$k<$linenumbers;$k++){
  $tstring="values=";
  for($i=0;$i<$numparams;$i++){
   $tstring="$tstring,$vals[$k][$i]";
  }
#print "$tstring\n";
  $tstring=~s/,//;
#print "$tstring\n";
  $pointname=$tstring;
#print "$pointname\n";
  $pointname=~s/values=/pointname=np/;
#print "$pointname\n";
  $pointname=~s/,/_/g;
  $pointname=~s/-/m/g;
#print "$pointname\n";
  $pn=$pointname;
#print "$pointname\n";
  $pn=~s/pointname=//;
  if($flag==1){
   print "collecting results for $pn...\n";
   `./results.pl -d $workingdir -p $pn$paramfile`;
  }elsif($flag==2){
   `rm $workingdir/$pn$paramfile`
  }else{
   system("cp $workingdir/$paramfile $workingdir/$pn$paramfile");
   system("perl -pi -e 's/^values=.+/$tstring/g' $workingdir/$pn$paramfile");
   system("perl -pi -e 's/^pointname=.+/$pointname/g' $workingdir/$pn$paramfile");   print "working on numeric point $pn...\n";
   `./justnumerics.pl -d $workingdir -p $pn$paramfile`;
  }
 }
}




sub readmultiparams {
my $locparamfile=$_[0];
my $lines=0;
my %hash_varloc = ();
my @array_loc1=();
my @array_loc2=();
open (EREAD,$locparamfile) || die "cannot open $locparamfile";
while(<EREAD>) {
  chomp;
  s/^\s+//;
  unless (/^#/) {
    s/\s+//g; 
    if(m/^paramfile=(.*)/i){$hash_varloc{"paramfile"}=$1
    }elsif(m/^lines=\s*(\d+)/){$hash_varloc{"lines"}=$1
    }elsif(m/^minvals=(.*)/i){$hash_varloc{"minvals"}=$1
    }elsif(m/^maxvals=(.*)/i){$hash_varloc{"maxvals"}=$1
    }elsif(m/^stepvals=(.*)/i){$hash_varloc{"stepvals"}=$1
    }elsif(m/^values.*=(.*)/i){push (@array_loc2,$1)
    }elsif(m/=/i){print "Warning - invalid assignment $_ in $locparamfile\n"
    }else{push (@array_loc1,$_)};
  }
}
if(@array_loc1){$hash_varloc{"points"}=join(';',@array_loc1)};
if(@array_loc2){$hash_varloc{"values"}=join(';',@array_loc2)};
return %hash_varloc;
}

 
