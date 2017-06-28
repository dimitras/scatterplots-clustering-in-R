#!/usr/bin/env ruby
ifile1 = ARGV[0]

#qvalues: Truseq:11 Pico:22 V4:33
##Pico_V4
# qvalue_x = 22
# qvalue_y = 33
##Pico_Truseq
# qvalue_x = 22
# qvalue_y = 11
##V4_Truseq
qvalue_x = 33
qvalue_y = 11

#first range
qvalue_x_min = -2
qvalue_x_max = 0
qvalue_y_min = -10
qvalue_y_max = -6
#second range
qvalue_x_max2 = -10
qvalue_y_min2 = -4
qvalue_y_max2 = 0

File.open(ifile1,"r").each_line do |line|
	line.strip!
	if line.start_with?("\"id\"")
		puts line
		next
	end

	qx = line.split("\t")[qvalue_x].to_f
	qy = line.split("\t")[qvalue_y].to_f

	if Math.log(qx) < qvalue_x_max && Math.log(qx) > qvalue_x_min && Math.log(qy) < qvalue_y_max && Math.log(qy) > qvalue_y_min
		puts line
	end

	if Math.log(qx) < qvalue_x_max2 && Math.log(qy) < qvalue_y_max2 && Math.log(qy) > qvalue_y_min2
		puts line
	end

end


