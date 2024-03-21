
-- set these, it will keep low latency from start to end hour (6am to 10pm by default)

starthr=6
endhr=22

function sleep(n)
   os.execute("sleep " .. tonumber(n))
end

latencybytes = "0x000001F4" -- the number 500 in hex, represents 500 us latency target
-- according to: https://www.kernel.org/doc/html/latest/power/pm_qos_interface.html we can write hex string

dmafile = io.open("/dev/cpu_dma_latency","wb")
isopen = true
dmafile:write(latencybytes);

while true do
   sleep(60);
   now = os.time();
   nowtable = os.date("*t",now);
   if (not isopen) and nowtable["hour"] >= starthr and nowtable["hour"] <= endhr then
      dmafile = io.open("/dev/cpu_dma_latency","wb");
      dmafile:write(latencybytes);
      isopen = true
   elseif isopen and ( nowtable["hour"] < starthr or nowtable["hour"] > endhr ) then
      dmafile:close()
      isopen = false
   end
end

   
