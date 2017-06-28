#!/usr/bin/env ruby
ifile1 = ARGV[0]

File.open(ifile1,"r").each_line do |line|
	line.strip!
	if line.start_with?("\"id\"")
		puts [line, "Gene is OFF"].join("\t")
		next
	end

	id = line.split("\t")[0]
	
	#for Pico
	higroup1 = line.split("\t")[1..6].map{|x| x.to_i}
	higroup2 = line.split("\t")[23..28].map{|x| x.to_i}
	lowgroup = line.split("\t")[12..17].map{|x| x.to_i}
	higroup1min = higroup1.min.to_i
	higroup2min = higroup2.min.to_i
	lowgroupmax = lowgroup.max.to_i
	overall_min = [higroup1min,higroup2min].min.to_i

	# puts higroup1.inspect
	# puts higroup1min
	# puts higroup2.inspect
	# puts higroup2min
	# puts overall_min
	# puts lowgroup.inspect
	# puts lowgroupmax

	if lowgroupmax < 100 && overall_min > 200
		rank = overall_min - lowgroupmax
		puts [line, "Pico"].join("\t")
	end

	#for Truseq
	lowgroup  = line.split("\t")[1..6].map{|x| x.to_i}
	higroup2 = line.split("\t")[23..28].map{|x| x.to_i}
	higroup1 = line.split("\t")[12..17].map{|x| x.to_i}
	higroup1min = higroup1.min.to_i
	higroup2min = higroup2.min.to_i
	lowgroupmax = lowgroup.max.to_i
	overall_min = [higroup1min,higroup2min].min.to_i

	if lowgroupmax < 100 && overall_min > 200
		rank = overall_min - lowgroupmax
		puts [line, "Truseq"].join("\t")
	end

	#for V4
	higroup2 = line.split("\t")[1..6].map{|x| x.to_i}
	lowgroup = line.split("\t")[23..28].map{|x| x.to_i}
	higroup1 = line.split("\t")[12..17].map{|x| x.to_i}
	higroup1min = higroup1.min.to_i
	higroup2min = higroup2.min.to_i
	lowgroupmax = lowgroup.max.to_i
	overall_min = [higroup1min,higroup2min].min.to_i

	if lowgroupmax < 100 && overall_min > 200
		rank = overall_min - lowgroupmax
		puts [line, "V4"].join("\t")
	end

end


