#using Pkg
#Pkg.activate(".")
#Pkg.add(["DataFrames","StatsPlots","CSV"])

using DataFrames, StatsPlots, CSV
gr()

site = "www.google.com" # site to traceroute to
nsec = 120 # number of seconds to ping for


function gethops(dname)

    trin = Pipe()
    trout = Pipe()
    trerr = Pipe()
    trproc = run(pipeline(`traceroute -n -w 0.5 $dname`; stdin=trin,stdout=trout,stderr=trerr),wait=false)

    hops = DataFrame()
    @async (wait(trproc);close(trout));
    readline(trout) ## throw away the first line
    while true
        if(!eof(trout))
            line = lstrip(readline(trout));
            s = split(line,r"[ *]+")
            push!(hops,(n=s[1],addr=s[2],ms=s[3]))
            #@show hops
        else
            break;
        end
    end
    return hops
end



function pinghops(hops,dur)

    pids = [];
    pipes = [];
    data = DataFrame();
    endplot = false
    
    @sync begin
        for (i,r) in Iterators.enumerate(eachrow(hops))
            push!(pipes,Pipe())
            push!(pids,run(pipeline(`ping -n -D -w $(dur) $(r.addr)`,pipes[end]),wait=false))
            @async begin
                while !eof(pipes[$i])
                    l = readline(pipes[$i])
                    m = match(r"\[(.*)] .* icmp_seq=([0-9]*) .* time=([0-9.]*) ms",l)
                    m === nothing || push!(data,(time=parse(Float64,m[1]),dest=$(r.addr),seq=m[2],ms=parse(Float64,m[3])))
                end
            end
        end
        plottask = 
            @async begin
                sleep(3)
                while !endplot
                    sleep(2)
                    p = plot(data.time .- minimum(data.time),data.ms,group=data.dest)
                    display(p)
                end
            end
        for i in pids
            wait(i)
        end
        endplot = true
        for i in pipes
            close(i)
        end
    end
    data
end


pingdata = pinghops(gethops(site),nsec)

#@df pingdata plot((:time - minimum(:time)), :ms,group=:dest)
