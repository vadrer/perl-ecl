use strict;
use blib;
use ecl;
use Tcl::Tk;

my $int = new Tcl::Tk;
$int->export_tcl_namespace;

$int->Eval(<<'EOS');
pack [frame .frtop] -side top -fill x -expand 0
pack [button .frtop.b1 -text b1 -command perl::b1] -side left
pack [text .t] -side top -fill x -expand 0
EOS
my $t = $int->widget('.t','Text');

my $cl = new ecl;

$cl->eval(<<"EOS");
(defun si::universal-error-handler (cformat eformat &rest args)
  (prin1 (format nil "hej-hoj-huj [c=~A] [e=~A] ~A" cformat eformat args)))
EOS


$t->insertEnd($cl->eval("(format nil \"~A\" (expt 2 1000))")."\n");
my $list = $cl->eval("'(a b c d qwerty)")->_tie;

for (@$list) {$t->insertEnd(qq/$_\n/)}

sub tcl::b1 {print "hehe\n";}

$int->MainLoop;

