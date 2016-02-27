# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl Language-Lisp-ECL.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More ('no_plan'); # tests => 30?;
BEGIN { use_ok('Language::Lisp::ECLs') };

#########################

my $cl = new Language::Lisp::ECLs;

ok($cl->eval_string("(+ 1 2)")==3, '1+2=3');
is($cl->eval_string("(format nil \"[~S]\" 'qwerty)"), '[QWERTY]');
is($cl->eval_string("'qwerty")->stringify, '#<SYMBOL COMMON-LISP-USER::QWERTY>', 'symbol stringification');
is($cl->eval_string("(defpackage :qw)")->stringify, '#<PACKAGE QW>', 'package');
is($cl->eval_string("(defpackage \"qw\")")->stringify, '#<PACKAGE qw>', 'package');

my $lam = $cl->eval_string("(lambda (x y) (+ x y))");
is($lam->funcall(40,2),42,'funcall');

my $lamstr = $cl->eval_string("(lambda (name) (format nil \"hello mister ~A\" name))");
is($lamstr->funcall("Twister"),"hello mister Twister",'funcall');

# autoloading
$cl->eval_string(<<"EOS");
(defun this (a)
  (make-string a :initial-element #\\t))
EOS
is($cl->this(5),'t'x5);
is($cl->this(50_000),'t'x50_000);

my $nil = $cl->eval_string("nil");
is($cl->format($nil,"[[~A]]","qwerty"),"[[qwerty]]");

# list as tied array
my $list = $cl->eval_string("'(a b c \"d\" qwerty)");
my $aref = $list->_tie;
is($#$aref,4);
is($aref->[3],"d");
is($aref->[-2],"d");

