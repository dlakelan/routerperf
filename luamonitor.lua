#!/usr/bin/lua


luamonitordb="/tmp/sqmmonitor.db"



require("posix")
sqlite3 = require("luasql.sqlite3")

-- returns a posixTimespec, which has two components tv_sec and tv_nsec (nanoseconds)
function timenow()
   return posix.time.clock_gettime(posix.time.CLOCK_REALTIME)
end


function getpingstats(ip,packsize)
   local f = io.popen("ping -c 5 -i 0.2 -s "..packsize .. " "..ip)
   local dat = f:read("*line") -- ignore the first line
   local l = {}
   for i=1,5,1 do
      l[i] = f:read("*line")
      if(l[i]) then
	 l[i] = string.gsub(l[i],".*time=([0-9.]*) ms.*","%1")
      else
	 error("pingfail")
      end
   end
   table.sort(l) -- in place sort of ping times
   return({l[1],l[3],l[5]}) -- least median and upper value
   
end


function put_db_obs(conn,now,addr,packsize,cpustats,netstats,pingstats,cpustats2,netstats2,after)

   -- insert cpu and ping delay stats...
   err = conn:execute("insert into stats values(" .. now.tv_sec .. "," .. now.tv_nsec .. ",'" .. addr .. "'," .. packsize .. "," ..
		   cpustats["user"] .. "," .. cpustats["nice"].. "," .. cpustats["sys"] .. "," .. cpustats["idle"] .. ",".. cpustats["io"] .. "," .. cpustats["irq"] .. "," .. cpustats["softirq"] .. "," .. cpustats["steal"] .. "," .. cpustats["guest"] .. "," .. cpustats["gnice"] .. "," ..
		   pingstats[1] .. "," .. pingstats[2] .. "," .. pingstats[3] .. "," ..
		   cpustats2["user"] .. "," .. cpustats2["nice"].. ",".. cpustats2["sys"] .. "," .. cpustats2["idle"] .. ",".. cpustats2["io"] .. "," .. cpustats2["irq"] .. "," .. cpustats2["softirq"] .. "," ..  cpustats2["steal"] .. "," .. cpustats2["guest"] .. "," .. cpustats2["gnice"] .. "," ..
		   after.tv_sec.. "," .. after.tv_nsec .. ");")

--   print(err);
   -- insert network bandwidth stats for all devices, indexed by now.tv_sec and device name
   for intfc,data in pairs(netstats) do
      conn:execute("insert into ifstats values(" .. now.tv_sec .. "," .. now.tv_nsec.. ",'" .. intfc .."',".. data .. "," .. netstats2[intfc]..");");
   end
end


function readcpus(f)
   local cpuline = f:read("*line")
   -- the first line is a cpu line with user, nice, system, idle, iowait,irq,softirq, steal, guest, guestnice
   local arr = {}
   for i in string.gmatch(cpuline,"%S+") do
      table.insert(arr,i)
   end
   return ( {user=arr[2],nice=arr[3],sys=arr[4],idle=arr[5],["io"]=arr[6],irq=arr[7],softirq=arr[8],steal=arr[9],guest=arr[10],gnice=arr[11]} )
   
end

function readnetstats(f)
   local netline = f:read("*line")
   netline = f:read("*line")
   -- discard header lines, now read one line per net device
   local arr = {}
   netline = f:read("*line");
   while netline do
      netline = string.gsub(netline,"^[\t ]*","")
      netline = string.gsub(netline,":? +",",")
      s,e,dev,rest = string.find(netline,"([^,]*),(.*)")
--      print(dev)
--      print(rest)
      arr[dev] = rest
      netline=f:read("*line");
   end
   return(arr)
end



function main ()

   math.randomseed(os.time())
   
   local db = sqlite3.sqlite3()
   local conn = db:connect(luamonitordb)
   conn:execute("create table if not exists stats (secs integer primary key, nsecs int, addr varchar(255), packsize int, cpuuser float, cpunice float, cpusys float, cpuidle float, cpuio float, cpuirq float, cpusoftirq float, cpusteal float, cpuguest float, gnice float, ping1 float, pingmed float, ping5 float, cpuuser2 float, cpunice2 float, cpusys2 float, cpuidle2 float, cpuio2 float, cpuirq2 float, cpusoftirq2 float, cpusteal2 float, cpuguest2 float, gnice2 float, endsecs integer, endnsecs integer); ")

   conn:execute("create table if not exists ifstats (secs int, nsecs int, iface varchar(255), rbytes int, rpack int, rerr int, rdrop int, rfifo int, rframe int, rcompr int, rmulti int, tbytes int, tpack int, terr int, tdrop int, tfifo int, tcolls int, tcarrier int, tcompr int, rbytes2 int, rpack2 int, rerr2 int, rdrop2 int, rfifo2 int, rframe2 int, rcompr2 int, rmulti2 int, tbytes2 int, tpack2 int, terr2 int, tdrop2 int, tfifo2 int, tcolls2 int, tcarrier2 int, tcompr2 int);")

   local ips = {"google-public-dns-a.google.com","google-public-dns-b.google.com",
		"fbcdn.net","akamai.com", "cloudflare.com",
		"one.one.one.one"}
   
   while true do
      pcall(function ()
	    ip = ips[math.floor(math.random()* # ips)+1]
	    packsize = math.floor(math.random()*1400) +64
	    sleeptime = {tv_sec=10+math.floor(math.random() * 10 ),tv_nsec=0};
	    --print("getting data")
	    posix.time.nanosleep(sleeptime);
	    now = timenow();
	    st = io.open("/proc/stat","r")
	    nd = io.open("/proc/net/dev","r")
	    st:setvbuf("no")
	    nd:setvbuf("no")
	    cpustats = readcpus(st)
	    netstats = readnetstats(nd)
	    ok,pingstats = pcall(function() return getpingstats(ip,packsize) end)
	    if(not ok) then error("pingfail") end
	    st = io.open("/proc/stat","r")
	    nd = io.open("/proc/net/dev","r")
	    st:setvbuf("no")
	    nd:setvbuf("no")
	    cpustats2 = readcpus(st)
	    netstats2 = readnetstats(nd)
	    after = timenow();
	    put_db_obs(conn,now,ip,packsize,cpustats,netstats,pingstats,cpustats2,netstats2,after)
	    
      end)
   end
end

main()

