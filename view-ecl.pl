use strict;
use blib;
use ecl;
use Tcl::Tk;

my $int = new Tcl::Tk;
$tcl::e2='(progn (prin1 "qtr\n") (prin1 (/ 2 100)) )';
$int->export_tcl_namespace;

$int->Eval(<<'EOS');
pack [frame .frtop] -side top -fill x -expand 0
pack [button .frtop.b1 -text {b1 (F8)} -command perl::b1] -side left
pack [entry .frtop.e2 -textvariable perl::e2] -side left -expand 1 -fill x
pack [button .frtop.b2 -text {eval (F9)} -command perl::b2] -side left
pack [text .t] -side top -fill both -expand 1
focus .frtop.e2

bind . <F8> {.frtop.b1 invoke}
bind . <F9> {.frtop.b2 invoke}

wm geometry . +200+10
EOS
my $t = $int->widget('.t','Text');

  chdir '/Personal2/sci/maxima-5.37.3/src'; # either before ecl creation, or do it in lisp level

my $cl = new ecl;

1 and $cl->eval(<<"EOS");
(defun si::universal-error-handler (cformat eformat &rest args)
  (progn
    (prin1 (format nil "hej-hoj-huj [c=~A] [e=~A] ~A" cformat eformat args))
    ;;(tpl-print-current)
    (invoke-debugger t)
    nil
  ))
EOS


$t->insertEnd($cl->eval("(format nil \"~A\" (expt 2 1000))")."\n");
my $list = $cl->eval("'(a b c d qwerty)")->_tie;

for (@$list) {$t->insertEnd(qq/$_\n/)}

sub tcl::b1 {
  print "hehe\n";
  $cl->eval("(load \"maxima-build.lisp\")");
    $cl->eval("(maxima-load)");
=ignore
    steps to do:
  (in-package :maxima)
  (initialize-runtime-globals)
    run 'continue', feed to it string, which 

=cut
    #$cl->eval("(cl-user::run)");
  print STDERR "h0\n";
    $cl->eval("
      (progn
        (in-package :maxima)
        (initialize-runtime-globals))
    ");
  print STDERR "h2\n";
  #$cl->eval("(continue (make-string-input-stream \"2+3;\"))");
    print $cl->eval("(toplevel-macsyma-eval \"2+3;\")");
    print '>>'.$cl->eval("\$__"),";\n";
    print '>>'.$cl->eval("(symbol-value \$__)"),";\n";
    #print '>>'.$cl->eval("%o1"),";\n";
  print STDERR "h3\n";
    #$cl->eval("");
  print "hoho\n";
}

$int->MainLoop;

sub tcl::b2 {
  $t->insertEnd("> $tcl::e2\n".
    $cl->eval($tcl::e2)
    ."\n");
  $t->seeEnd;
}
