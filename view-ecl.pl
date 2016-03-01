use strict;
use blib;
use ecl;
use Tcl::Tk;

my $int = new Tcl::Tk;
my $mw = $int->mainwindow;

$int->Eval(<<'EOS');
pack [frame .frtop] -side top -fill x -expand 0
pack [button .frtop.b1 -text b1] -side left
pack [text .t] -side top -fill x -expand 0
EOS
my $t = $int->widget('.t','Text');

my $cl = new ecl;

$cl->eval_string(<<"EOS");
(defun si::universal-error-handler (cformat eformat &rest args)
  (prin1 (format nil "hej-hoj-huj [c=~A] [e=~A] ~A" cformat eformat args)))
EOS


$t->insertEnd($cl->eval_string("(format nil \"~A\" (expt 2 1000))"));
my $list = $cl->eval_string("'(a b c d qwerty)");


$int->MainLoop;

