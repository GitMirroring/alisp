#!/usr/bin/perl -w
use strict;

# Copyright (C) 2022 Andrea G. Monaco
# 
# This file is part of al.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#


# Run test.pl a lot of times and gather the results



use IPC::Open2;



my $total_runs = 32;

my $failed_runs = 0;


my @lines;


for (my $run = 0; $run < $total_runs; $run++)
{
    my $pid = open2 (my $test_out, my $test_in, './test.pl');

    while (<$test_out>)
    {
	push (@lines, $_);
    }

    my @last_line = split (' ', $lines [-1]);

    if ($last_line [2] ne "(100%),")
    {	
	print "run " . ($run + 1) . " failed\n";

	print "last 10 lines of output:\n-----\n";

	for (my $i = -10; $i < 0; $i++)
	{
	    print $lines [$i];
	}

	print "-----\n\n\n";

	$failed_runs++;
    }
}


if ($failed_runs > 0)
{
    print "\n";
}


print "total runs: " . $total_runs . "\n";
print "passed " . ($total_runs - $failed_runs) . " (" . ($total_runs - $failed_runs) / $total_runs * 100 . "%), failed " . $failed_runs . "\n";
