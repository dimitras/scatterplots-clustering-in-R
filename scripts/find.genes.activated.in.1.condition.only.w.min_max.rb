#!/usr/bin/env ruby
ifile1 = ARGV[0]

File.open(ifile1,"r").each_line do |line|
	line.strip!
	if line.start_with?("\"id\"")
		puts [line, "Gene is ON"].join("\t")
		next
	end

	id = line.split("\t")[0]
	
	#for Pico
	lowgroup1 = line.split("\t")[1..6].map{|x| x.to_i}
	lowgroup2 = line.split("\t")[23..28].map{|x| x.to_i}
	highgroup = line.split("\t")[12..17].map{|x| x.to_i}
	lowgroup1max = lowgroup1.max.to_i
	lowgroup2max = lowgroup2.max.to_i
	highgroupmin = highgroup.min.to_i
	overall_max = [lowgroup1max,lowgroup2max].max.to_i

	# puts lowgroup1.inspect
	# puts lowgroup1max
	# puts lowgroup2.inspect
	# puts lowgroup2max
	# puts overall_max
	# puts highgroup.inspect
	# puts highgroupmin

	if overall_max < 100 && highgroupmin > 200
		rank = highgroupmin - overall_max
		puts [line, "Pico"].join("\t")
	end

	#for Truseq
	highgroup  = line.split("\t")[1..6].map{|x| x.to_i}
	lowgroup2 = line.split("\t")[23..28].map{|x| x.to_i}
	lowgroup1 = line.split("\t")[12..17].map{|x| x.to_i}
	lowgroup1max = lowgroup1.max.to_i
	lowgroup2max = lowgroup2.max.to_i
	highgroupmin = highgroup.min.to_i
	overall_max = [lowgroup1max,lowgroup2max].max.to_i

	if overall_max < 100 && highgroupmin > 200
		rank = highgroupmin - overall_max
		puts [line, "Truseq"].join("\t")
	end

	#for V4
	lowgroup2 = line.split("\t")[1..6].map{|x| x.to_i}
	highgroup = line.split("\t")[23..28].map{|x| x.to_i}
	lowgroup1 = line.split("\t")[12..17].map{|x| x.to_i}
	lowgroup1max = lowgroup1.max.to_i
	lowgroup2max = lowgroup2.max.to_i
	highgroupmin = highgroup.min.to_i
	overall_max = [lowgroup1max,lowgroup2max].max.to_i

	if overall_max < 100 && highgroupmin > 200
		rank = highgroupmin - overall_max
		puts [line, "V4"].join("\t")
	end

end


