#!/usr/bin/env ruby
ifile1 = ARGV[0]

File.open(ifile1,"r").each_line do |line|
	line.strip!
	if line.start_with?("\"id\"")
		puts [line, "Gene is ON"].join("\t")
		next
	end

	id = line.split("\t")[0]
	rank_Pico = line.split("\t")[40].to_i
	rank_V4 = line.split("\t")[41].to_i
	rank_Truseq = line.split("\t")[42].to_i

	if (rank_Pico.abs > 1500) && (rank_V4.abs < 100) && (rank_Truseq.abs < 100)
		puts [line, "Pico"].join("\t")
	end
	if (rank_Pico.abs < 100) && (rank_V4.abs > 1500) && (rank_Truseq.abs < 100)
		puts [line, "V4"].join("\t")
	end
	if (rank_Pico.abs < 100) && (rank_V4.abs < 100) && (rank_Truseq.abs > 1500)
		puts [line, "Truseq"].join("\t")
	end
end


