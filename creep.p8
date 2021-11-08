pico-8 cartridge // http://www.pico-8.com
version 33
__lua__
--the laboratories of dr. creep
-- by gpi 2021

--[[
change-log
- changed way-pole-split
- changed menu-main-scroll-limit
----
- optimized frankenstein 
  movment - should be closer
  to the original
- main menu - first lab is 
  selected by default
- change how doors are drawn on
  the map
- easy-mode option for death
 
--]]

--[[

sfx
0,1 trap
2 death
3,4 con
5 lma-switch
6-13 frc field
14 fankenstein
15 frc field off
16-23 transmitter-settings
24 teleport
25 mummystart
26 gunshoot
27 door open
28 pickup key
--]]


readmatrix=split("1,bel,444,2,loc,4443,3,key,344,4,con,24444,5,lma,441,5,swi,4418,6,frc,4444,7,trp,44441,8,tmc,344,8,tmd,044,9,mum,4444,10,frk,441,11,gun,4444441",",",false)

_saveitems=split "con,dir,3,lma,3,1,swi,3,1,trp,5,1,tmc,1,3"

forcecolor=split "7,7,15,15,10,10,15,15"
colors=split "11,3,6,5,8,2,14,2,10,9,12,13,4,2,0,1,13,1,15,14,9,4,3,5,5,0"

playertxt=split "one player,two players"
easymodetxt=split "easy mode,hard mode"
lad_pol=split "lad,pol"

trp_spr=split "0x30,0x31,0x31,0x32,0x32,0x33,0x33,0x02"
frc_spr=split "0x7a,0x79,0x78,0x77,0x76,0x5f,0x5e,0x5d,0x5c"

tutorialtxt=split([[
doors and doorbells

one way to open a door is
to use the doorbell.
operate the doorbell by
standing in front of it and
pressing the ❎ button.
to go out an open door, stand
in front of the door and
press ⬆️ or ❎.
;ladders

to get on a ladder, stand
directly behind it and push
⬆️ or ⬇️. you can get off of
a ladder whenever your feet
are even with the front of a
walkway.
;sliding poles

to get on a sliding pole,
stand directly in front of
it and pull ⬇️. you cannot
climb up the sliding poles.
you can get off of a pole
whenever your feet are even
with the middle of a walkway.
;lightning machine

turn the sparks on or off by
standing in front of the
switch and push ⬆️,⬇️,❎.
;force field

turn off the force field by
standing in front of the
control and pushing ❎.
the force field will remain
off for a few seconds after
the button is released.
;mummy

when you run in front of
the ankh, the mummy slides
out of its tomb. mummies are
too stiff to use ladders or
sliding poles.
;locks and keys

to pick up a key, stand in
front of it and push ❎.
you can open a lock only
after you have picked up a
key of matching color. stand
in front of the lock and
push ❎.
;ray gun

operate the gun by standing
in front of the control and
using ⬆️ and ⬇️ to aim and
❎ to fire. the gun fires
automatically when it is at
the same level as a player.
;matter transmitter

stand in the booth and hold
⬆️ until the color in
the booth matches the color
of the desired receiver oval.
push ❎ to transmit.
;trapdoor

the trapdoor opens or closes
whenever anyone runs in front
of the control.
;frankensteins monster

when you walk in front of
his coffin, frankensteins
monster awakens and begins
to chase you. frankensteins
monster is able to use the
ladders and sliding poles.
;conveyor belt

to change the motion of the
conveyor belt, stand in front
of the control and push ❎.
;the exit door

the door at the right leads
out of the laboratory.
when you go out this door
you have escaped.
]],";")

-->8
-- tape

function tape_rewind()
  block_load ("levels",true)
  tape_pos,tape_end,tape_bit=unsplit"0xbd00,0x0000,0x8000"
end

function tape_rewind_save()
  tape_pos,tape_end,tape_bit=unsplit"0x4300,0x4400,0x8000"
end

function tape_bits(x,byte)
  local write,ret=0.5<<x,0
  for i=1,x do
    if byte then 
      -- overwrite (clears also bits!)
      poke2(tape_pos, byte&write!=0 and %tape_pos | tape_bit or %tape_pos & ~tape_bit)
    else
      if (%tape_pos & tape_bit !=0) ret |=write
    end
    tape_bit >>>=1
    write >>>=1
    if (tape_bit<1) tape_pos+=2 tape_bit=0x8000
  end
  return ret
end

function tape_setstr(str)
  tape_bits(4,#str)
  for i=1,#str do
    tape_bits(5,ord(str,i)-96)
  end
end

function tape_getstr()
  local ret=""
  for i=1,tape_bits(4) do
     ret ..= chr(tape_bits"5" +96)
  end
  return ret
end

function tape_find_laby(name)
  tape_rewind()
  while tape_read_laby() do
    if (laby.name == name) return true
  end
end

function _readtable(lin)
  local c={}
  for i=1,#lin do
    add(c,tape_bits(sub(lin,i,i)))
  end
  return c
end

function _read(what,lin,correct)
  for a=1,tape_bits(4) do
    add(room[what],_readtable(lin))
    if (correct and room[what][a][1]==0) room[what][a][1]=16
  end
end

function tape_read_laby()
  local n,c,r,d= tape_getstr()
  if (n=="") laby=nil return false

  

  laby={
    room = {},
    door = {},
    player =split"1,1,1,2,1",
    name = n
  }
   
  for ii=1,tape_bits"5" do
    room= {visited=false}
    for x in all(split "way,lad,pol,bel,loc,key,con,lma,swi,frc,trp,tmc,tmd,mum,frk,gun") do
      room[x]={}
    end
   
    room.col,room.x,room.y,room.w,room.h=tape_bits"4",tape_bits"4",tape_bits"4",tape_bits"3",tape_bits"3"
  
    _read("way","444",true)
    
    for p in all(room.way) do
      p.type,p.box = "way",{
        p[2]*8+2,
        p[3]*8,
        (p[2]+p[1])*8-1,
        p[3]*8+7
      }
    end
    _read("lad","444",true)
    for p in all(room.lad) do
       p.type,p.box="lad",{
         p[2]*8+2,
         p[3]*8,
         p[2]*8+5,
         (p[3]+p[1]-1)*8
       }
    end
    _read("pol","444",true)
    for p in all(room.pol) do
      p.type,p.box="pol",{
        p[2]*8+2,
        p[3]*8,
        p[2]*8+5,
        (p[3]+p[1]-1)*8
      }
    end
 
    repeat
      c=tape_bits(4)
      for i=1,#readmatrix,3 do
        if (c==tonum(readmatrix[i])) _read(readmatrix[i+1],readmatrix[i+2])
      end
    until c==0
   foreach(split([[bel,switchbox,2,3,2,3,5,10
loc,switchbox,2,3,2,3,5,10
key,box,2,3,2,3,5,10
con,switchbox,4,5,2,3,5,10
con,movebox,2,3,2,0,12,7
lma,deathbox,1,2,1,0,6,15
swi,switchbox,1,2,2,3,5,10
frc,switchbox,3,4,2,3,5,10
frc,frcbox,1,2,2,0,5,15
trp,switchbox,3,4,2,3,5,10
trp,deathbox,1,2,2,0,5,7
tmc,box,2,3,3,1,15,12
mum,switchbox,3,4,2,3,5,10
frk,homebox,1,2,3,2,4,15
gun,switchbox,5,6,2,3,5,10]],"\n"),function (data)
    data=split(data)
    for a in all(room[data[1]]) do
      a[data[2]]={
        a[data[3]]*8+data[5],
        a[data[4]]*8+data[6],
        a[data[3]]*8+data[7],
        a[data[4]]*8+data[8]
      }
    end
  end)


 for p in all(room.con) do
      p.anim,p.dir=0,0
      if (p[1]==2) p.dir=1
      if (p[1]==3) p.dir=3      
    end
    for p in all(room.trp) do
      p.intouch=false
    end
    
    for d in all(room.tmd) do
      d.x,d.y = d[2]*8+4,d[3]*8+8
      d.obj = findway(d.x,d.y)
    end
    
    for m in all(room.mum) do
      m.sx,m.sy,m.anim,m.x,m.cx,m.y,m.state,m.type,m.speed,m.killer = m[1]*8+8,m[2]*8+8,0,0,0,0,"home","mum",0.4,true
   m.obj = findway(m.sx,m.sy)
      setbox(m)
    end
    
    for f in all(room.frk) do
      f.sx,f.sy,f.anim,f.x,f.cx,f.y,f.dir,f.state,f.type,f.speed,f.killer = f[1]*8+4+(f[3]==1 and 1 or -3),f[2]*8+8,0,0,0,0,f[3],"home","frk",0.50,true
      setbox(f)
    end
    
    for a in all(room.gun) do
      if (a[1]==0) a[1]=16
      a.x,a.y,a.min,a.max,a.speed,a.shootbox,a.shootspeed = a[2]*8,(a[3]+a[4])*8,a[3]*8,(a[3]+a[1]-1)*8,0.25,{},(a[7]==0 and 4 or -4)
   a.x2=a.x + (a[7]==0 and 8 or -8)
    end
    --
    add(laby.room,room)
  end
  --door
  repeat
    c=tape_bits"5"
    if (c==0) break
    d=_readtable "0445"
 d[1],d.open=c,false
    if d[1]==d[4] then
      d[5],d[6]=d[2],d[3]
    else
      d[5],d[6]=tape_bits"4",tape_bits"4"
    end
    add(laby.door,d)
  until false
  --playerdata
  laby.player=_readtable "54444"
  return true
end

-->8
-- misc

function msets(x,y,w,h,s)
  for dx=0,w-1 do
    for dy=0,h-1 do
      mset(x+dx,y+dy,s+dx+dy*16)
    end
  end
end

function box2box(a,b)
  return not (a[3] < b[1] or
     a[1] > b[3] or
     a[4] < b[2] or
     a[2] > b[4])
end

function distance(a,b)
  local x,y = (a.x-b.x)/8, (a.y-b.y)/8
  return x*x+y*y -- we don't need exact values!
end

function timestr(t)
  return t\60 ..":".. sub("0"..t%60\1,-2)
end

function print_msg(x)
  msg,msg_anim=x,0
end

function clear_menuitem()
  foreach (split("1,2,3,4,5"),menuitem)
end

function show_map() 
  if (state=="show_map") state=save_state else  save_state,state=state,"show_map" 
end

function setbox(t)
 t.box={
  t.x\1-2,
  t.y\1-3,
  t.x\1+1,
  t.y\1+4
 }
end

-->8
--savestate

function game_load()
  memcpy( unsplit"0x4300,0x5e00,0x100")
  _load()
  oldstate=""
end
function gamestate_load()
  oldstate=""
  local oldp=player
  _load()
  for p in all(player) do
    p.time = oldp[p.type].time+60*5
  end
end

function _load()
  -- load saved lab
  tape_rewind_save()
  tape_find_laby(tape_getstr())
  -- need to rewind
  tape_rewind_save()
  tape_getstr()
  -- how many players?
  num_player=tape_bits"1"+1
  -- init level
  laby_init()

  -- player
  for p in all(player) do
    p.time,p.x,p.y,p.r,p.state = tape_bits"16"|tape_bits"16">>>16,tape_bits"4" *8+4,tape_bits"4" *8,tape_bits"5",p.r==0 and "quit" or "alive"
  end

  -- collected keys
  for i=1,tape_bits"4" do
    add(player.key,tape_bits"3")
  end

  -- doors
  for d in all(laby.door) do
    d.open= tape_bits"1"==1
  end

  --rooms  
  for r in all(laby.room) do
    r.visited=tape_bits"1"==1
    for i=1,#_saveitems,3 do
      for c in all(r[_saveitems[i]]) do
        c[_saveitems[i+1]]=tape_bits(_saveitems[i+2])
      end
    end

    --key picked
    for k in all(r.key) do
      k.picked=tape_bits"1"==1
    end
   
    --mum
    function load_mon(what)
      for m in all(r[what]) do
        m.x,m.y,m.state = tape_bits"4" *8+4,tape_bits"4" *8, "alive"
        m.cx=m.x
        if (what=="frk") m.dir=tape_bits "3"
        if (m.x==4 and m.y==0) m.state="home"
        if (m.x==124 and m.y==0) m.state="death"
        setbox(m)
      end
    end
    load_mon "mum"
    --frk
    load_mon "frk"
    --gun
    for g in all(r.gun) do
      g.y=tape_bits"4"*8
    end
  end
end

function game_checkpoint()
  tape_rewind_save()
  --laby-name
  tape_setstr(laby.name)
  --how many players
  tape_bits(1,num_player-1)
  --playerdatas
  for p in all(player) do
    tape_bits(16,p.time )
    tape_bits(16,p.time<<16)
    tape_bits(4,p.x\8)
    tape_bits(4,p.y\8)
    tape_bits(5,p.state!="quit" and p.r or 0)
  end  
  --collected keys
  tape_bits(4,#player.key)
  for k in all(player.key) do
    tape_bits(3,k)
  end
  --doors
  for d in all(laby.door) do
    tape_bits(1,d.open and 1 or 0)
  end
  --rooms
  for r in all(laby.room) do
    tape_bits(1,r.visited and 1 or 0)
    for i=1,#_saveitems,3 do
      for c in all(r[_saveitems[i]]) do
        tape_bits(_saveitems[i+2], c[_saveitems[i+1]])
      end
    end
  
    --key
    for k in all(r.key) do
      tape_bits(1,k.picked and 1 or 0)
    end
    
    --mum
    function save_mon(what)
      for m in all(r[what]) do
        local x,y = m.cx\8,m.y\8
        if m.state=="home" then
          x,y=0,0
        elseif m.state!="alive" then
          x,y=15,0
        end
        tape_bits(4,x)
        tape_bits(4,y)
        if (what=="frk") tape_bits(3,m.dir)
      end
    end
    save_mon "mum"
    --frk
    save_mon "frk"
    --gun
    for g in all(r.gun) do
      tape_bits(4,g.y\8)
    end
  end
  if (tape_pos>=tape_end) poke(0x4300,0)
end

-->8
--draw helper
function draw_lma(m)
  local y=m[2]-1
  while fget(mget(m[1],y),7) and y>0 do
    mset(m[1],y,0x34 + m[3]*0x24)
    y-=1
  end
end


function laby_draw_map()
  memset(unsplit"0x2000,0,0x1000")
  local off,d
  --way
  for p in all(room.way) do
    for x=p[2],p[2]+p[1]-1 do
      mset(x,p[3],0x02)
    end
    mset(p[2]+48,p[3],0x01)
    mset(p[2]+47+p[1],p[3],0x03)
  end
  --lad
  for p in all(room.lad) do
    for i=p[3],p[3]+p[1]-2 do
      if (mget(p[2],i)==2) mset(16+p[2],i,0x14)
      mset(p[2],i,0x04)
    end
    mset(16+p[2],p[3]+p[1]-1,0x24)
  end
  --pol
  for p in all(room.pol) do
    for i=p[3],p[3]+p[1]-2 do
      if mget(p[2],i)==2 then
        mset(p[2],i,0x15)
        mset(32+p[2],i,0x7b)
      else
        mset(p[2],i,0x05)
      end
    end
    mset(16+p[2],p[3]+p[1]-1,0x25)
  end
 -- door
 doors={}

 for i = 1,#laby.door do
   d=laby.door[i]
     
   if d[1]==roomnb or d[4]==roomnb then
     add(doors,d)
     off= d[1]==roomnb and 1 or 4
     d.ox,d.oy,d.sx,d.sy,d.room = d[off+1]*8+8,d[off+2]*8+8,d[6-off]*8+8,d[7-off]*8+8,d[5-off]
     d.box,d.col,d.anim = {d.ox-4,d.oy-4,d.ox+3,d.oy+3},d[1]==d[4] and 5 or laby.room[d.room].col,d.open and 8 or 0
     msets(d[off+1]+16,d[off+2],2,2,d[1]==d[4] and 0x08 or 0x06)
   end
 end
 --bel
 for b in all(room.bel) do
   b.col=doors[b[1]].col
   --mset(b[2],b[3],0x4f+d.col)
 end
 --key
 for k in all(room.key) do
   if (not k.picked) mset(k[2],k[3],0x25+k[1])
 end
 --con
 for c in all(room.con) do
   d=0x4d
   if (c.dir==1) d=0x4e
   if (c.dir==3) d=0x4f
   mset(c[4],c[5],d)
 end
 --lma
 foreach(room.lma,draw_lma)
 --swi
 for s in all(room.swi) do
   mset(s[1],s[2],0x49+s[3])
 end
 --frc
 for f in all(room.frc) do
   mset(f[1],f[2],0x0d)
   f.activ,f.delay=1,0
 end
 --trp
 for t in all(room.trp) do
   t.anim=#trp_spr * t[5]
 end
 --tmc
 for t in all(room.tmc) do
   msets(t[2]+32,t[3],2,2,0x10)
   t.delay,t.tele=0,0
 end
 --tmd
 for c=1,#room.tmd do
   room.tmd[c][1]=c
 end
 --mum
 for m in all(room.mum) do
   msets(m[1]+16,m[2],2,2,m.state=="home" and 0x0e or 0x2e)
   mset(m[3],m[4],0x23)
   add(creatures,m)
 end
 --frk
 for f in all(room.frk) do
   msets(f[1]+16,f[2],1,2,0x0a)
   msets(f[1]+32,f[2],1,2,0x0b + f[3])
   add(creatures,f)
 end
 --gun
 for g in all(room.gun) do
   for y=g[3],g[3]+g[1]-1 do
     mset(g[2],y,0x4b+g[7])
   end
   g.shoot=false
 end

 laby_draw_anim_map()
end

function laby_draw_anim_map()
  --traps
  for t in all(room.trp) do
    t.anim=mid(1,#trp_spr,t.anim + (t[5]==0 and -1 or 1))
    if (room.col!=8) mset(t[1],t[2],trp_spr[t.anim])
    mset(t[3],t[4],0x5a+t[5])
  end

  --con
  for c in all(room.con) do
    if (c.dir==1) c.anim+=2
    if (c.dir==3) c.anim-=2
    if (c.anim<0) c.anim+=8
    if (c.anim>7) c.anim-=8
    if (room.col!=8) msets(c[2],c[3],2,1,0x36+c.anim)
  end

  --animate powerline
  local adr,save=3040, peek4(3040)
  for i=0,6 do
    poke4(adr,peek4(adr-64))
    adr-=64
  end
  poke4(adr,save)
end

function draw_topbar(str,c,right)
  rectfill(0,0,128,6,c)
  print(str,1,1,7)
  --if (type(right)=="number") right=tostr(right)
  if (right~= nil) print(right,(128-4*#right),1,7)
end

function dyinganim(p)
  p.stateanim+=1
  for i=1,15 do
    pal(i,rnd(3)+5)
  end
  spr(p.spr,p.x-4,p.y-3,1,1,p.flp)
  pal()
  if (p.stateanim>=8) p.state,p.x,p.y="death",0,0 setbox(p)
end

function draw_player_door(p,s,flp)
  p.anim=flr(p.anim+1)%4
  p.spr=0x60+p.anim
  if (p.type==2) pal(15,4)
  sspr((p.spr & 0xf)*8,
       (p.spr>>4)*8,
       8,8,
       p.door.ox-4+s,p.door.oy-3-s/4,
       8-s,8-s,
       true)
end

function mypal(x)
  -- color walkway, bells, and so on
  pal(12,colors[x*2-1])
  pal(13,colors[x*2])
end

function unsplit(x) 
  return unpack(split(x))
end

function draw_map()
  --background transport chamber
  for t in all(room.tmc) do
    rectfill(t.box[1],t.box[2],t.box[3],t.box[4],t.tele>0 and rnd(colors) or colors[t[1]*2-1])
  end

  mypal(room.col)
  map(unsplit"16,1,0,8,16,15,1")--back
  map(unsplit"0,1,0,8,16,15,1")--mid
  pal(1,0)
  map(unsplit"16,1,0,8,16,15,8")--inmid
  map(unsplit"48,1,0,8,16,15,8")--inmid - waycorrect
  

  pal()
  --without palette
  map(unsplit"0,1,0,8,16,15,4")
  --shifted
  map(unsplit"0,1,0,11,16,15,2")

  --forcefield  
  pal(15, forcecolor[frc_anim])
  for f in all(room.frc) do
    if (f.activ<=1) spr(0x1d,f[1]*8,f[2]*8,1,2)
    spr(frc_spr[f.activ],f[3]*8,f[4]*8+3)
  end

  --doors
  for d in all(doors) do
    mypal(d.col)
    spr(0x12,d.box[1],d.box[2])
    clip(d.box[1],d.box[2],8,8)
    spr(0x13,d.box[1],d.box[2]+d.anim)
    clip()
  end
 
  function _item(s,typ,col,off)
    for a in all(room[typ]) do
	     mypal(a[col])
	     spr(s,a[2]*8,a[3]*8+3-off)
	   end
  end

  --door bell
  _item(unsplit"0x50,bel,col,0")
  --locks
  _item(unsplit"0x59,loc,4,0")
  --tele dest
  _item(unsplit"0x75,tmd,1,3")

  pal()

  function _getspr(f,way,lad,pol)
    f.flp=false
    if f.obj.type=="way" then
      f.spr=way+flr(f.anim)
      f.flp= f.dir==➡️
    elseif f.obj.type=="lad" then
      f.spr=lad+flr(f.anim)
    elseif f.obj.type=="pol" then
      f.spr=pol
    end
  end
  
  --frankenstein
  for f in all(room.frk) do
    if f.state=="home" then
      spr(0x44,f[1]*8+(f[3]==1 and 1 or -3),f[2]*8+6,1,1,f[3]==1,false)
    elseif f.state=="alive" then
      _getspr(f,0x45,0x51,0x55)
      spr(f.spr,f.x-4,f.y-3,1,1,f.flp)
    elseif f.state=="dying" then
      dyinganim(f)
    end
  end

  --gun
  for g in all(room.gun) do
    --draw the ray, even when it hit something
    if g.shoot or g.shootbox[1]~=nil then
      line (g.shootbox[1],g.shootbox[2],g.shootbox[3],g.shootbox[4],10) 
      if (not g.shoot) g.shootbox[1]=nil
    end
    spr(0x6a+ g.y%2,g.x, g.y,1,1,g[7]==0,false)
    spr(0x69,g.x2,g.y,1,1,g[7]==0,false)
    spr(g.spr,g[5]*8,g[6]*8+3)
  end

  --mummy
  for m in all(room.mum) do
    if m.state=="alive" then
      _getspr(m,0x65)
      spr(m.spr,m.x-4,m.y-3,1,1,m.flp)
    elseif m.state=="init" then
      clip(m[1]*8,m[2]*8+5,0x0c,0x0f)
      spr(0x64,m.sx+8-4-m.stateanim,m[2]*8+m.stateanim)
      clip()
      if flr(m.stateanim)%2 == 0 then
        pal(5,12)
        pal(1,5)
        spr(0x23,m[3]*8,m[4]*8+3)
        pal()
      end
    elseif m.state=="dying" then
      dyinganim(m)
    end
  end

  --player
  for p in all(player) do
    if p.state=="alive" then
      _getspr(p,0x60,0x70,0x74)
      if (p.type==2) pal(15,4)
      spr(p.spr,p.x-4,p.y-3,1,1,p.flp)
      pal()
    elseif p.state=="dying" then
      dyinganim(p)
    elseif p.state=="indoor" then
      p.stateanim+=1
      draw_player_door(p,p.stateanim,false)
      if (p.stateanim>=8) p.state="nothere"
    elseif p.state=="outdoor" then
      p.stateanim+=1
      draw_player_door(p,9-p.stateanim,true)
      if (p.stateanim>=8) p.state="alive"
    end
  end

  --lightning
  for m in all(room.lma) do
    spr(0x35,m[1]*8,m[2]*8-3)
    if (m[3]==1) spr(0x6c+rnd(4),m[1]*8,m[2]*8,1,2)
  end

  --foreground
  mypal(room.col)
  map(unsplit"32,1,0,8,16,15,1")--front
  pal()

end

function drawtopdefault()
  draw_topbar(timestr(player[1].time),2,player[2]!=nil and timestr(player[2].time) or "" )
end

-->8
-- _draw

function _draw()
  if draw[oldstate]!=nil then
    draw[oldstate]()
  end
  
  if msg!="" then
    local y=0
    if msg_anim<8 then
      y=-8+msg_anim
    elseif msg_anim>=30 and msg_anim<38 then
      y=-msg_anim+30
    elseif msg_anim>=38 then
      y,msg=-8,""
    end
    rectfill(0,y,128,y+7,8)
    print(msg,64-#msg*2,y+1,15)
    msg_anim+=1
  end
end

draw={}

function draw.menu()
  cls()
  map(0,0x20)
  draw_topbar("the laboratories of dr. creep",8)

  local y=10
  for i=max(menu_pos-15,1),menu_pos-1 do
    print(menu_text[i],1,y,7)
    y+=6
  end
  y+=1
  --rectfill(0,y-1,128,y+5,9)
  print(menu_text[menu_pos],2-1,y-1,4)
  print(menu_text[menu_pos],2,y,9)
  y+=7
  for i= menu_pos+1,#menu_text do
    print(menu_text[i],1,y,7)
    y+=6
  end

  print("written by gpi",128-14*4,128-12,1)
  print("original by ed hobbs",128-20*4,128-6,1)
end

function draw.room_play()
  cls()
  local x,y,c=64-4.5*#player.key,14,10
  
  drawtopdefault()
  
  for k in all(player.key) do
    spr(0x25+k,x,0)
    x+=9
  end

  draw_map()

  if laby.name=="tutorial" then
    for l in all(split(tutorialtxt[roomnb],"\n")) do
      print(l,64-print(l,0,-10)\2,y,c)
      y+=6
      c=9
    end
  end 
end

function draw.laby_exit_anim()
  cls()
  map(0,0x20) 
  drawtopdefault()
 
  for p in all(player) do
    if p.state=="walk1" or p.state=="walk2" then
      p.spr,p.flp=0x60+flr(p.anim),true
    else
      p.spr,p.flp=0x56+flr(p.anim%2),false
    end
    if (p.type==2) pal(15,4)
    spr(p.spr,p.x-4,p.y-3,1,1,p.flp)
    pal()
  end

end

function draw.show_map()
  cls()
  
  drawtopdefault()
  
  for r in all(laby.room) do
    r.xx,r.yy,r.ww,r.hh,r.cx,r.cy=r.x*8,r.y*8,(r.x+r.w)*8-1 , (r.y+r.h)*8-1,r.x*8+r.w*4,r.y*8+r.h*4
    if (r.visited) rect(r.xx,r.yy,r.ww,r.hh,7)
  end
 
  function _line(x1,y1,x2,y2,c)
    for x=-1,1 do
      for y=-1,1 do
        line(x1+x,y1+y,x2+x,y2+y,c)
      end
    end
  end
 
  for d in all(laby.door) do
    local r1,r2=laby.room[d[1] ],laby.room[d[4] ]
    if r1!=r2 then
      local x1,x2,y1,y2 = mid(r1.xx,r1.ww,r2.xx),mid(r2.xx,r2.ww,r1.ww),mid(r1.yy,r1.hh,r2.yy),mid(r2.yy,r2.hh,r1.hh)
      --printh( abs(y1-y2).." "..abs(x1-x2) )
      local dx,dy=abs(x1-x2),abs(y1-y2)
      if dx>1 and dy<2 then
        x1=(x2+x1)/2
        x2=x1
      elseif dx<2 and dy>1 then
        y1=(y2+y1)/2
        y2=y1
      end
      _line(x1,y1,x1,y2,0)

    end
  end  

 --[[
  for d in all(laby.door) do
    local r1,r2=laby.room[ d[1] ],laby.room[ d[4] ]
    if (r1.visited or r2.visited) _line(r1.cx,r1.cy,r2.cx,r2.cy,0)
  end  
  --]]
  
  for r in all(laby.room) do
    if (r.visited) rectfill(r.xx+1,r.yy+1,r.ww-1,r.hh-1,colors[r.col*2-1])
    if (player[1].state!="quit" and laby.room[player[1].r]==r) print("웃",r.cx-4,r.cy-3,room.col==10 and 7 or 15)
    if (num_player==2 and player[2].state!="quit" and laby.room[player[2].r]==r) print("웃",r.cx-3,r.cy-2,4)
  end
  print("you are here웃",71,123,15) 
end

-->8
--init & update
function _init()
  -- register permanent memory
  cartdata "3011c98f_908a_471f_ab56_3509c90b9850_creep"
  -- enable high memory
  poke(0x5f36,16)
  -- copy packed data in high
  memcpy(unsplit"0x8000,0x0000,0x4300")--,"creepbuild.p8")
  -- get content
  create_block()
  -- erase "rom"-part
  memset(unsplit"0x0000,0,0x4300")
  -- load gfx-data
  block_load "gfx"
  block_load "gfx_flags"

  easymode,suicide,num_player,frc_anim,msg,msg_anim,labylist,state
  =true   ,false  ,1         ,0       ,"" ,0       ,{}      ,"main_init"
  
  tape_rewind()
  while tape_read_laby() do
    add(labylist,laby.name)
  end
  
end

function _update()
  frc_anim= frc_anim % #forcecolor +1
  
  oldstate=state
  update[state]()
end

function laby_init()
  player,activeplayer,state = {key={}},0,"room_init"
  for i=1,num_player do
   add(player,{
    dir=0,
    d=nil,
    speed=1,
    type=i,
    state="new",-- initalize to start
    box=split "0,0,0,0",
    anim=0,
    isplayer=true,
    time=0
   })
  end
end


update={}

function update.main_init()
  --function get_save_info()
  if dget"0"==0 then
    save_state_txt= ""
  else
    memcpy(0x4300,0x5e00,0x100)
    tape_rewind_save()
    save_state_txt = tape_getstr().." ("..playertxt[tape_bits"1"+1]..")"
  end  
  
  state="main"
  block_load "mainback"
  block_load "music"
  music(0)  
end

function update.main()
  menu_text,state,menu_pos={},"menu",5
  add(menu_text,playertxt[num_player])
  add(menu_text,easymodetxt[easymode and 1 or 2])
  add(menu_text,save_state_txt)
  add(menu_text,"")
  for i in all(labylist) do
    add(menu_text,i)
  end
  --disable all submenus
  clear_menuitem()
end

function update.main_selected()  
  if menu_pos==1 then
    num_player=num_player % 2+1
    menu_text[1],state = playertxt[num_player],"menu"
    return
  elseif menu_pos==2 then
    easymode=not easymode
    menu_text[2],state = easymodetxt[easymode and 1 or 2],"menu"
    return
  elseif menu_pos==3 and save_state_txt!="" then
    game_load()
  elseif menu_pos>4 then
    tape_find_laby(menu_text[menu_pos])
    laby_init()
  end

  music(-1)
  block_load "sfx"   
  menuitem(4,"suicide",function()
    suicide=true
    for p in all(player) do
      if (p.state=="alive") die_creature(p)
    end
  end)
  menuitem(2,"save",function()
    if @0x4300!=0 then 
      memcpy(0x5e00,0x4300,0x100)
      print_msg "game saved"
    else
      print_msg "can't save, too complex lab"
    end
  end)
  menuitem(3,"load",game_load)
  if (easymode) menuitem(5,"reset room",gamestate_load)
  --menuitem(1,"map", show_map)
end

function update.room_init()
  local quitcount=0
  --revive player and quit-fix
  for p in all(player) do
   if p.state=="death" or p.state=="new" then
     if (p.state=="death") p.time+=60*5
     p.x,p.y,p.r,p.door,p.state = laby.player[p.type*2]*8+4,laby.player[p.type*2+1]*8,laby.player[1],nil,"alive"
   elseif p.door!=nil and p.door[1]==p.door[4] then
     p.state,p.door="quit",nil
   end
   if p.state=="quit" then
     if(num_player==2) p.r=player[3-p.type].r 
  quitcount+=1
   end
  end

  if quitcount==num_player then
    state = "laby_exit"
  else
   activeplayer = activeplayer % num_player +1
   roomnb = player[activeplayer].r
   room=laby.room[roomnb]
   room.visited,creatures,state = true,{},"room_play"
   laby_draw_map()
   
   --attach player to walkway
   for p in all(player) do
    if p.state!="quit" then
      if p.r == roomnb then
        add(creatures,p)
        setbox(p)
        p.obj,p.state = findway(p.x,p.y),p.door==nil and "alive" or "outdoor"
      else
        p.state="nothere"
      end
      p.stateanim=0
    end
   end
 end
 
 -- easymode - suicde
 suicide=false
 
 game_checkpoint()
end

function update.room_play()
  local s,dx,dy,con,move,p,y
  -- animations
  laby_draw_anim_map()

  -- players in room?
  s=true
  for p in all(player) do
    if (p.state!="quit" and p.state!="nothere" and p.state!="death") s=false
    
    --easymode check
    if (p.state=="death" and easymode and not suicide) gamestate_load() return
    
  end
  if (s) state="room_init"
 
  -- gun reset control
  for a in all(room.gun) do
    a.ctrl=-1
  end

  -- update player
  for p in all(player) do
    if p.state=="alive" then
      p.time+=0.0333 -- 1/30 clock update
      dx,dy,con=0,0,p.type-1
      local btn⬆️,btn⬇️,btn⬅️,btn➡️,btn❎ = btn(⬆️,con),btn(⬇️,con),btn(⬅️,con),btn(➡️,con),btnp(❎,con)
      if (btn⬅️) dx=-p.speed
      if (btn➡️) dx=p.speed
      if (btn⬆️) dy=-p.speed
      if (btn⬇️) dy=p.speed

      if (dx!=0 or dy!=0) update_anim(p,domove(p,dx,dy))

      if (btnp(🅾️,con)) show_map()
   
      --can control gun?
      for a in all(room.gun) do
        if box2box(p.box, a.switchbox) then
          if (btn⬆️) a.ctrl=⬆️
          if (btn⬇️) a.ctrl=⬇️
          if (btn❎) a.ctrl=❎
          if (a.ctrl==-1) a.ctrl=-2 -- set to manual control
        end
      end
      --control transmitter
      for c in all(room.tmc) do
        if box2box(c.box,p.box) then
          if btn⬆️ or btn⬇️ then
            if c.delay<=0 then
              c.delay,c[1] = 20,(c[1] % #room.tmd) +1
              sfx(15+c[1])
            end
          elseif btn❎ then
            p.x,p.y,p.obj,c.tele=room.tmd[c[1]].x,room.tmd[c[1]].y,room.tmd[c[1]].obj,15
            setbox(p)
            sfx"24"
          end
        end
      end
      --control lma-switch
      for a in all(room.swi) do
        if btn❎ or (btn⬆️ and a[3]==1) or (btn⬇️ and a[3]==0) then
          if box2box(a.switchbox, p.box) then
            a[3]^^=1
            mset(a[1],a[2],0x49+a[3])
            s=0.5
            for i=1,min(#room.lma,8) do
              s<<=1
              if (a[4]&s!=0) room.lma[i][3]^^=1 draw_lma(room.lma[i])
            end
            sfx"5"
          end
        end
      end

      if btn❎ or btn⬆️ then
        --enter door
        for d in all(doors) do
          if (d.open and box2box(d.box, p.box)) p.state,p.stateanim,p.door,p.x,p.y,p.r = "indoor",0,d,d.sx-2+p.type*2,d.sy,d.room del(creatures,p)
        end
        --pick key
        for k in all(room.key) do
          if not k.picked and box2box(k.box,p.box) then
            add(player.key,k[1])
            k.picked=true
            mset(k[2],k[3],0)
            sfx"28"
          end
        end
        --use doorbel
        for b in all(room.bel) do
          if (not doors[b[1]].open and box2box(b.switchbox, p.box)) doors[b[1]].open=true sfx"27"
        end
        --unlock
        for l in all(room.loc) do
          if not doors[l[1]].open and box2box(l.switchbox, p.box) then
            for k=1,#player.key do
              if player.key[k]==l[4] then
                doors[l[1]].open=true
                sfx"27"
                deli(player.key,k)
                break
              end
            end
          end
        end	  
        --control conveyer
        if not btn⬆️ then
          for c in all(room.con) do
            if box2box(c.switchbox, p.box) then
              c.dir,s=(c.dir+1)%4,0x4d
              if (c.dir==1) s=0x4e
              if (c.dir==3) s=0x4f
              mset(c[4],c[5],s)
              sfx(s==0x4d and 3 or 4)
            end
          end
        end
        --control force field
        for a in all(room.frc) do
          if (box2box(a.switchbox,p.box)) a.activ,a.delay=#frc_spr,30 sfx"15"
        end
      end
    end
  end

  -- update gun
  for a in all(room.gun) do
    -- overwrite with shoot?
	for p in all(player) do
	  if (not a.shoot and a.ctrl==-1 and p.box[2]<=a.y+3 and a.y+3<=p.box[4]) a.ctrl=❎
	end
    -- move gun?
    p,a.spr = get_near_player(a),0x40
    if p~=nil then
      y=p.y-3
      if (a.ctrl==⬇️ or (a.ctrl==-1 and y > a.y)) a.y+=a.speed a.spr=0x42
      if (a.ctrl==⬆️ or (a.ctrl==-1 and y < a.y)) a.y-=a.speed a.spr=0x41
      a.y=mid(a.min,a.max,a.y)
    end
    -- shoot?
    if (not a.shoot and a.ctrl==❎) a.spr,a.shootbox,a.shoot=0x43,{a.x2,a.y+3,a.x2+8,a.y+3},true sfx"26"
    -- move ray
    if a.shoot then
      a.shootbox[1]+=a.shootspeed
      a.shootbox[3]+=a.shootspeed
	  -- kill creatures
      for c in all(creatures) do
        if (box2box(c.box,a.shootbox)) die_creature(c) a.shoot=false
      end
	  -- stop on force field
      for f in all(room.frc) do
        if (f.activ<=1 and box2box(f.frcbox,a.shootbox)) a.shoot=false
      end
      -- outscreen
      if (a.shootbox[3]<0 or a.shootbox[1]>128) a.shoot=false
    end
  end
 
  --update frankenstein
  for a in all(room.frk) do
    if a.state=="home" then
      -- activate frankenstein?
      for p in all(player) do
        if p.state=="alive" and a.sy==p.y\1 and ((a[3]==1 and a.sx<=p.x) or (a[3]==0 and a.sx>=p.x)) then
          a.state,a.stateanim,a.x,a.cx,a.y,a.obj = "alive",0,a.sx,a.sx,a.sy,findway(a.sx,a.sy)
          setbox(a)
          sfx "14"
        end
      end
    elseif a.state=="alive" then
      -- secure that on a object!
      if (a.obj==nil) a.obj=findway(a.x,a.y)
      -- check if should change direction
      if cancross(a) then
        -- Check the distance on possible new positions
        move={}
        for p in all(player) do
          if p.state=="alive" then
            for i=0,3 do
              dx,dy=dirxy(i,8)
              add(move,{i,distance(p,{x=dx+a.x,y=dy+a.y})})
            end
          end
        end	
        for i=1,#move do
          -- sort
          for a=i+1,#move do
            if (move[a][2]<move[i][2]) move[i],move[a] = move[a],move[i]
          end
          -- try to step in the new direction
          a.dir=move[i][1]
          dx,dy=domove(a,dirxy(a.dir,a.speed))
          if (dx!=0 or dy!=0) update_anim(a,dx,dy) break
        end
      else
        -- move in the current direction
        if (a.obj.type=="pol") a.dir=⬇️
        dx,dy=domove(a,dirxy(a.dir,a.speed))
        update_anim(a,dx,dy)
        
        -- if can't move go back!
        if (dx==0 and dy==0) a.dir=backdir(a.dir)
      end
    end
  end
  
  -- update mummy
  for a in all(room.mum) do
    if a.state=="home" then
	  -- check if the player touch an ankh
      for p in all(player) do
        if p.state=="alive" and box2box(a.switchbox,p.box) then
          a.state,a.stateanim = "init",0
          sfx"25"
          msets(a[1]+16,a[2],2,2,0x2e)
        end
      end
    elseif a.state=="init" then
	  -- move mummy out of its tomb
      a.stateanim+=0.5
      if (a.stateanim>=8) a.x,a.cx,a.y,a.state = a.sx,a.sx,a.sy,"alive"
    elseif a.state=="alive" then
	  -- walk around
      p= get_near_player(a)
      if p~= nil then
        if (p.x\1>a.x\1) update_anim(a, domove(a,a.speed,0))
        if (p.x\1<a.x\1) update_anim(a, domove(a,-a.speed,0))
      end
    end
  end
  
  -- update transmitter chamber
  for a in all(room.tmc) do
    -- control delay
    if (a.delay>0) a.delay-=1
	-- teleport-animation
	if (a.tele>0) a.tele-=1
  end
 
  -- update force field
  for a in all(room.frc) do
    if a.activ>1 then
      a.delay-=1
      if (a.delay<=0) a.delay=30 a.activ-=1 sfx(14-a.activ)
    end
  end
   
  --check against creatures
  for c in all(creatures) do
  
    -- lighting machine deathbox
    for p in all(room.lma) do
      if (p[3]==1 and box2box(p.deathbox,c.box)) die_creature(c)
    end
    -- conveyor bell movement
    for p in all(room.con) do
      if box2box(c.box,p.movebox) then
        if (p.dir==1) domove(c,-1.1,0,false)
        if (p.dir==3) domove(c,1.1,0,false)
      end
    end
  end
 
  -- update trap
  for t in all(room.trp) do
    s=false
    for c in all(creatures) do
	  -- something touches the switchbox
      if (box2box(c.box,t.switchbox)) s=true
	  -- deathbox check
      if (t.anim<=1 and box2box(t.deathbox,c.box)) die_creature(c)
    end
    if s and not t.intouch then
      t[5]^^=1
      sfx(t[5])
    end
    t.intouch=s
  end
  
  -- door-animation
  for d in all(doors) do
    if (d.open and d.anim<8) d.anim+=1
  end
end

function update.menu()
  function _dox(dy)
    repeat
      menu_pos+=dy
    until menu_text[menu_pos]!=""
  end

  if (btnp "2" and menu_pos>1) _dox"-1"
  if (btnp "3" and menu_pos<#menu_text) _dox"1"

  if (btnp "4" or btnp "5") state="main_selected"

end

function update.laby_exit()
  for p in all(player) do
    p.x,p.y,p.anim,p.state,p.stateanim = -32*p.type,111,0,"walk1",0
  end
  state="laby_exit_anim"
  block_load "endscreen"
  clear_menuitem()
end

function update.laby_exit_anim()
  local out=0
  for p in all(player) do
    p.anim=(p.anim+p.speed) % 4
    if p.state=="walk1" then
      p.x+=p.speed
      if (p.x>60-p.type*8) p.state="wink"
    elseif p.state=="wink" then
      p.stateanim+=1
      if (p.stateanim>20) p.state="walk2"
    else
      p.x+=p.speed
      if (p.x>150) out+=1
    end
  end
  if (out == num_player) state="main_init"
end

function update.show_map()
  for p=0,1 do
    if (btnp(❎,p) or btnp(🅾️,p)) state=save_state
  end
end

-->8
--game helper
function cancross(t)
  if t.obj.type=="way" then
    if (t.cx == t.oldcx) return false
    t.oldcx=t.cx 
  
    for s in all(lad_pol) do
      for l in all(room[s]) do
        if box2box(t.box,l.box) and t.x\1 == l.box[1]+2 then 
          if (s!="pol" or l.box[4]\1 != t.y\1) return true
        end
      end
    end
    
  else
    for w in all(room.way) do
      if (box2box(t.box,w.box) and t.y\1 == w.box[2]) return true
    end
  end
  return false
end

function findway(x,y)
  for s in all(split"way,lad,pol") do
    for w in all(room[s]) do
      if (w.box[1]<=x and x<=w.box[3] and w.box[2]<=y and y<=w.box[4]) return w
    end
  end
  --return nil
end

function dirxy(dir,speed)
  if dir==0 then
    return -speed,0
  elseif dir==1 then
    return speed,0
  elseif dir==2 then
    return 0,-speed
  end
  return 0,speed
end

function backdir(dir)
  if dir==0 then
    return 1
  elseif dir==1 then
    return 0
  elseif dir==2 then
    return 3
  end
  return 2
end

function domove(t,dx,dy,dir)
  local oldx,oldy,oldobj,mask,doreset = t.x,t.y,t.obj,t.type==1 or t.type==2,false
  
  dir = dir==nil and true or false
  if (dir) mask=true
  -- change object?!
  if t.obj.type=="way" then
    -- move on lad or pol
    if dy!=0 then
      for s in all(lad_pol) do
        for l in all(room[s]) do
          if box2box(t.box,l.box) then
            if t.x\1 < l.box[1]+2 then
              if (dx==0) dx=t.speed
            elseif t.x\1 > l.box[1]+2 then
              if (dx==0) dx=-t.speed
            else
              t.obj,dx = l,0
            end
          end
        end
      end
    end
  else
    -- move to walkway
    if dx!=0 then
      for w in all(room.way) do
        if box2box(t.box,w.box) then
          if t.y\1 < w.box[2] then
            if (dy==0) dy=t.speed
          elseif t.y\1 > w.box[2] then
            if (dy==0) dy=-t.speed
          else
            t.obj,dy = w,0
          end
        end
      end
    end
  end

  if t.obj.type=="way" then
    -- walk on way
    t.x=mid(t.obj.box[1],t.x+dx,t.obj.box[3])
    if (dir) t.dir= dx<0 and ⬅️ or ➡️

  elseif t.obj.type=="lad" or t.obj.type=="pol" then
    -- climb on pol/lad
    if (t.obj.type=="pol" and dy<0) dy=0
    t.y=mid(t.obj.box[2],t.y+dy,t.obj.box[4])
    if (dir) t.dir= dy<0 and ⬆️ or ⬇️

  end

  -- if a collision already exist, walk through it!  
  -- collision with other creature?
  for c in all(creatures) do
    if (c==t) mask=true -- don't block all creatures
    c.oldcol=mask and box2box(c.box,t.box)
    if (c.type=="frk") c.oldhomecol=box2box(c.homebox,t.box)
  end
  -- or force-field
  for f in all(room.frc) do
    f.oldcol=f.activ<=1 and box2box(f.frcbox,t.box)
  end
 
  -- update collision box
  setbox(t)
 
  -- check collision with enemies/friends 
  for i,c in pairs(creatures) do
    if c!=t and not c.oldcol and box2box(c.box,t.box) then
      if (c.isplayer and t.killer) die_creature(c)
      if (c.killer and t.isplayer) die_creature(t)
      doreset=true
    end
  end
  -- traps block
  for f in all(room.trp) do
    if (f.anim<=1 and box2box(f.deathbox,t.box)) doreset=true
  end
  -- force field block
  for f in all(room.frc) do
    if (not f.oldcol and f.activ<=1 and box2box(f.frcbox,t.box)) doreset=true
  end
  -- frankenhome
  for f in all(room.frk) do
    if (not f.oldhomecol and box2box(f.homebox,t.box)) doreset=true
  end
  -- reset position if collision with something
  if (doreset) t.x,t.y,t.obj=oldx,oldy,oldobj setbox(t)

  -- remember last position in the middle of a tile
  local xx=t.x/8%1
  if (xx>0.4 and xx<0.6) t.cx=t.x\1
        
  return t.x-oldx,t.y-oldy
end

function die_creature(c)
  if (c.state=="alive") c.state,c.stateanim="dying",0 del(creatures,c) sfx"2"
end

function get_near_player(a)
  local d,i,d2=0x7fff
  for p in all(player) do
    if p.state=="alive" then
      d2=distance(p,a)
      if (d2<d) i=p d=d2
    end
  end
  return i
end

function update_anim(p,a,b)
  if (a<0) a=-a
  p.anim+=a+b
  if (p.anim<0) p.anim+=4
  if (p.anim>=4) p.anim-=4
end

-->8
--compression

function decompress(dest,src)
  local dictionary, bitsize,odest,w,entry = {}, 9,dest
  tape_pos,tape_bit=src+2,0x8000
  
  for i = 0, 255 do
    dictionary[i] = chr(i)
  end
  
  w=dictionary[tape_bits(8)]
  poke(dest,ord(w)) dest+=1
  for i = 2, %src do
    ii=tape_bits(bitsize)
    entry = dictionary[ ii] or w .. sub(w, 1, 1)
    
    -- poke in combination with "multi"-ord is buggy - don't use it
    -- it will cause random crashes of pico-8!
    for i=1,#entry do
      poke(dest,ord(entry,i)) dest+=1
    end
 
    add(dictionary,w .. sub(entry, 1, 1))
    if (1<<bitsize<=#dictionary+1) bitsize+=1
 
    w = entry
  end
  return dest-odest
end

function block_load(x,dont)
  
  for i=1, #block,2 do
    local d=block[i+1]
    for a=1,#d,4 do
      if d[a]==x then
		if (block[i]!=oldcomp) decompress(0xbd00,block[i]) oldcomp=block[i]
        if (not dont) memcpy(d[a+2],d[a+1],d[a+3])
		break
      end
    end
  end
  
end


function create_block()
  local block_adr,s,v,e=0xbd00  
  block={}
  repeat
    v=%block_adr block_adr+=2
    if (v==0) break
    add(block,v)
    v=@block_adr block_adr+=1
    s=""
    for i=1,v do
      s..=chr(@block_adr) block_adr+=1
    end
    add(block,split(s))
  until false
 
end



__gfx__
2f700100800181000090124000180a0e1484b00030500a808704228290a028280a1a828b740360200298160480c262c130180f4c480208ca411128042135c400
2035d4a0108c4515d28002d32e500044c05cb2236018409a8d00402b61c1380cae2aeb84e14004813e38106128001224b219b16fa8d1661c952b6bce55bb3d6b
4bf6d6c5b079acd967ddd5ab9aee5505df2f0cd70ff5c185f06248d020d4831b65cb8173201000800d90356559d256cc953f6ec9d3f66cc91786cd8d9468a023
ae3da15765db957b7dfa57c6c2af07a46b5120cd0a4b6ad0b7f8fdfb8707d3c1446d69002750c59376e9d9f28edcaf3740d5b20c7dba171e85276ee7db710fdd
0f774bb9b2897ebca73f90d77501dd3b8fde2db7fa007df97f9ee7bf8fe7add7518308ef8ff110a00218a1832850870290a2ede69381382e6dbdbbd70482658a
5871891ab1261a68278d78f18f956cd426e6ed659872892ab22ab9a8c76920f9803aad9c9d46ad8dd822893eb32eb166d47b678d64c17813ddf8a1961904743e
b42a4aa7239d39f4715a1fdae1591b7f28df895e90625200a79d89f59162d5a84ee6a9988864902276e916a92d8e67242985a0887ec92d95d977997ac6edb9b9
e89bc836818e386c8a0a01a572a0a88a98a21a1aab942a68a19602a272c8a5aff879a1d98c968a1ad4767a3d8786562c662969abb9868d6a0a273a8ae5a4f806
81b2a2ad9d8891a2a8f907bafaa262fa297049258d260c2bc249a9823799868ec7d399ca61b4fa3c8fcef9a97e6a567136eab42edbd02ac6626a8bdb60007ade
568baaafea9a60eebbd00c526bbabbfeb1fa3f2cfacb5bb5eb2bb0b17a9e7ee6afb07b6d6af2df6fa6672c81d9a7b9fa8ea1b558a59b4c55bfe188d2d10cce69
20e4c81b91b6137c451b7ca0c12f3238588cb201ac62c72392ba2b8c39cd5c32cd1fb1bc506cb206ec63c73393be0c417fcafcf3d14b9330331d9ecb31bbda04
b4724b2d33dd4de21323a0045f4d35d55d75d15b137433dc37c38d541a33a0036f8d39d59d76ca5bf1fb4ce6b20acde6df6317bc730db0d52114da04b77e7bed
3fdd0e92cb23bb9a83fdbfd51e78d98b67fd833eb3eb7dd3ce531836635eb5eb9b81626625ce9b20b7ec8ed91f93a086af8e39e59e7aca6b6abba3bed9e83ebb
ed873b7b6f9e78d1de4bdbb1bbb5536d0617fea0f1cf3c30bb0fb3e12f2cf4c3bbd0975e72f50ffccaaf0dfb634ffcddcd7dfe6728735b9cb7fd6fc1e18f9e21
b76eb9fc4ddbe1eebb910c217bfabffef1fb3f3cfbcf7bf5efc4fe439fbe32fff7f4c79917f2d3bc8d62e91781bfcc87bd5f4c13007c15b580bba20e1c805283
0650714a0d2880668a8b2e833f022797d103249c54831690c132ecf451664b24404c461268f2200034718681169b53c9a078b1eb893cb097ce16c4f3ee033402
588f1e01544e0d88422e89442196462e9803e31d54615a812aa0554a9c6824478b54719b0f262048681364139c836091d6881dc0334a06d091b30236d8164c15
e233dd8136d11d8115e8e28a067ca1328c32b866c71f88247b8632f187ca1ff8636a0e6c223aa936090e691d8c64519e3ab166081da053af8184128b044c1198
c22494649c814a626d071cd843699618c1554a36c84a222394a4b4955ea296c62da1d2271d8832c10a4c21592c2dac03929652a2668a2d0043e5965881f58a2c
b1dd651a942376934aa136c500d803410174e2b102483179462090d4931b2422574b10008324050843008562b100a42b5a55ac0d1630e84500d9b62416f4c11a
0b72d816cc3df400db454873e98e0e31250095a053d1a13ed8d543043c006b92247961081bc8b76aade49b63ca64d898e44d1d18cb4a6081d52b45f9e3a3472d
84db400841edd92ca8e2351832536d054b20e258a79c40a3d800d86167327cb696806ac156434ba103c60d1074b3cf2559aea81725c3fb4e5072ba0598516587
0ea86210da6820a2641225431a02aa44e52a10aac8a9916495b1451e2aac691f15443c0b393596501c1a7b6b8f25a2eba758faf7c8116d035d974ea54553f0d9
53698935414c02c61b662f193d3b9c8e2e444b8faa614ba5456c412cc3a2fa6725677d03975bc24516cf5b3b2328b69214540e26616b1e2e5db4fc903da62a50
631b800bb4ad45b42bd6f8066fa8adf9801e54e2db5d612b0317867ca35ad3dac4bb5e65b8cc871b55f2e64e7100e213b59d467b413ac8c2e44b649ed5a3e286
e6dd2d71bd2448d534fb4f2dcb572f6bedbebbb4f2d73ad67feb69adb03cf65fdcfefb0c6d0d8513ddb8b207cd577978ff2eb36a087865031c90eb7bc48e5ebd
0361716b813cd076cb1678936f074cf17881261194c317987263815c517acb26b89567176cf27c81369196c317d8736b315cf2a7ccc8f8976f177cf30b3ac044
18c83009a6468d81748f1880f811e075f054ba283e2258cc2319d3e91f8c427a1d3e79ddee17b8a3049d5655b8c22f2975e6997ce6c1c668a97b8b349c7c38bc
908245193b62c4e291dc334ac550f89d663891f6b09954030acd403378ec833112bfcc46f8da67448cf33f9fc863ffcc23c9f461108cb23fcb8ef878e74ffce7
7f9786c250d02e0a56ed9facc3b9d14669b363302db675a7e80413de2b7ad5eba17d65b2ca7ef95b604b0974c0233e343bcb1b8a666ea12d93efd94609564833
ad0d7730ce7272d557ea076fa5dce3fb1dceb1898e67c0ccb6bf6e53309b4e7249e0b19c46f5d8de4a346a4b7c08bf1964078edb4a53dba72440c732dbfe7bb3
6733fccebbb9f6e63cd226e11a4f28110563ce5c3c99e687fd44fbbde6677bd360cbb666bdc0c3ece1ea3cd1788bfd612abfce83c2ec3073076bb3cd28bd808c
f2bb8f562169b71f37c5cd138b33be69b54ee67ae627a115fe3cc5b2722376295dd0571bb3febf4eb6cce6707175807ac9a92726d6f2bb119fbb3a6ed35e3a23
0a579cbce4a7c53535c35f53faed68b90dcbdde0aaf91967b1ab7fa78c44471b98e411a84c24d90ebbb8dbbde6606d9c01bba166bfdb47d63cdbcffd07f0d18c
9360ed77db3570c7fea531e7f67cf0f62e5913de596a65c58da0e1bb46c70ff7eac357c53f8ab75e0e20107907b9ff9f9eb4fb92e8eab9e3a822f51ed2668b08
7d8be5051eb8f2e7406c7e8b0fdf2fb3ec0638536cf6ed8f1df3fb645c5009d360cc0f06df9151f3cb4e6506fbc0ce36eac78b7be0872f1af4fbe71f28126803
3c013881364117c0bf1977f7acde31f7d7de5599e78e8cf4f5bee3f7cfc7daf7ff17c74c7c787864d71a8d7b72975768546f09c0e7acd08f7d08b71fa64006f7
40d68f744bc7f796e36c6ef7c7e30a7404a7a74471837f873688bc0f0cc0c0dcec0e0fc0d00d2d0103d0d04d5d0406d0d07d01887228641a02898cc708739879
62762754730603e71836f188812828e1e3648e08e7b43c0e83e758f43d7083d0c764248c805848357a728db258152e7c08585033e58586705865330d8e4836f4
568e7a68503e53050f487855078003e7784457840b686834b68905686835058d8d7878f418858288783897848a7878e7e68087888848288c86883078c7838178
d709d88306988844b68f817898f73304879898d8f489829898b9d98c0e983044330409a8403343038798a8684a838178a8a85b860868c03c0d7381b8b8b96b84
8eb8b8fb9345848898e9d8858098982c9c8f8698c88cdc8a8288c8cc1d8e80d8c81d934d12e7d8169c8486d840d52e738798a8ddda897da8362e5e8886e8e87e
9e888ae8e8bede8c8ee8e8fe000f59700800060483054100001a1003840644182511289426448315804a55aa244880080100540a08c1356a299465420c406466
050c204cb32218623a1c9003aa4099008c45239c22853146f973b13b8a29002ff25404893f262983235619a7729c0448100299025d700239049006630811c23b
89258c4190103cd82306405e2024aaa9a20840086ebce33b43003bb14794d52b0020c68d6107200876791558240a5108506b0807a2550ffee5d764907ce81f8e
d1b2000900d1da95c211eb57453ea205a92bc4a9bb94dbfac20cd781aad1d0100c2d71236202657abcaa5cac9d2108977cdcc44e8b63600b104e4dbb18a68caf
27c174b82f68009342c674f47e117347e2dbb94aadd80433d999d18a912a41b04787d3efe5ef3cbfd179f5ffe30bab111007700aa05b8974980b462f74f50cd4
d5578668e7206cfd00a09108133d03904da1ebd00637104c44957af9226f7104ef7a10106cd8cd0c9524a68e65d135e5411cfdd1204d0453836160055975676c
04f274d5f0491188dd11c1208df1dd8922b71c65091b831131dc0d1942914000061150558d14549819519452df1285830001000568668191b879de0391002a15
a6902883004e20aa3009d3632801604474354e7183676ed89704a9006a62a098bc20004b7760700000d0e83000006202097acd00cc05010d04047166f0a522a0
0984b31e166ab5653da943a3e2bb760c880280802228b3592ee8aa8600fa533c7a26444c356916880e811a1e176e11458606200b000bab300604969120230059
92e00a0e9282204c00f9a0a2e198ea806e75996c9662aee962142c9ed927067287207ae99f9c0a00b2202c68c2362e8604959a66f2e72419ac50202cc94130b0
41092016112c6800949d3cc210433c7853116ba11bdafb829951156ca195276cb445410216604275099c21727c28161f65042face56d66e03e910b1389581ac3
10710cc42473431d00331dcd594f34b8613d32d21df4521dc5d04f4deb67fcfba61b057b004d71dc0544d94bb432cc7da44d0dd3d93ac33a2300b0403dd4d267
f5b26f0d27d144e2d056109e6fe52fde0ef4e18338b0014980c4000b988e20b9710153e13e00e1ee1793090868120463007003950d2bfe207ea55685000800c2
304b9e5ae6afaa39a0ae8e10206a99a9160e9314895b79b02504ac2004500c21e0bb130cba24c7a9304400aaf8c31092a89e3080041bea0084e5585081409e50
f6d70885072ca7f242660171983c3000b6d3046ef8e361796082b917000de2d3554da641ce2baa3524ffd0518f7f304a021ec8f030006c700e0030020d083061
13084f04b6f5a7fe8596c2b02a0b5450318dba2804c27828fe688e065210236a19f0216b84a528004a48084b68803031b8f500ec8cfc1818020604a93eb001d4
094d71a322243d71622a80080008c06800005241e40000ce8800ce48b8d9a7fa06e49300000a85003000000622602dfbabc6c02e0234021544186022d908334a
ee5453647393c5022874d8028e9e000de750e975882eb43208490a74f1100e32100c6ae8a314609b00ae17408e096608137c0299100003fed69c2ca5729c8111
59ced6353e9900092f60208c00f6d00098730000008802a41c8108479184f0309b890106e905088550105ec272ca1081364f7e512ecc04f5d703503541dae62e
0c65d700063765208792d09c4a56a92b2d21c0b5bd000ee24eb101892f5093309f0b08407290c73f8cf9374be932284b3d69956a0560c2dc11a458436c33f487
9b23760227ca375984a24e9ba3fd4676e05bed1c05082f2294f870da48c8226d3ba423b0c68af99a50239b0d632f049b64144540b8e5881504a41526c8008119
15a98c8e823062d63102d312996353d9851e5a886c4f3d74219011ebff009ef4b251463241ba1e9e6a30c8609282062192c1745b41004a2e4362e4f696ae3a20
41224413d01b009164d31102b1944893c1b8518002a3ad8255ca23791e04680402162402a868853b5ccdf7e686ae9284b43a0cc5770010e033cd3975f5bfa922
fa83cd8cc8346da86000865459ca92a8bb8d00810a40ea9a8e625940cea5044867006d640806ba9541f2d3ba0b96265b455cba464e8547476de966c9a6f4f499
dcc93b26aa5d089abaa8a236742c0af3cde1055ac6814864866f00463206402d4dd159b88cc0038da2546428d1aaa86b29672a2b4f93e604f63073222ae18ac5
544a8eb27b00ae634d8ebf36f222074340dbf321bf34e17d9f1e7b20e592edcdbca0f6d47e5b219aa4a39bfdf7f216167b302d718d6ae34aae7b109ebe9040f7
2008335c496e691d6dfac9a57697f685855a554b7c29294052aa08803c04304c1e01c74b7a0dde4f450118604b2e471968e66420d73d69ff56ce9836659f4e72
313aca260435ee1b7cb24b8f458333c672c574eb9f4bb762cfe439f76e170af4a5986d6c03c4e6042cd19b74eac5ad321415da863a9168991dd881c680ed6ad9
40bbc230b65ea5882a9e6a6b2c950999e5304cb880029300000ecb7ef99f0e374002bf918ee3ffc12e112006a11d24721d760a906f3b009830a586947fc84b2a
08692002010129865a306f4f4df9e04096d2d662579a7a633292500803e893618508108e4336c80076d057da7b6aad6dc57edbb6fa576e57fccb3042a68595de
78aace20905d06740e8d82e0850c106a872a8840956a252b200028c7528d06b8d261cd28c53058b3e6055cd3779b7e62bdcd477a5be6a22cb20c6190c2120885
361b1c7bae68b1fdd73bd9f6018f37737190c80ec6e5cad8203c80800216d416650751b500881ea113c70f29000e837021f8c916e005e71f0c03008d3ef157c9
0b823400978cf221000639f837270ca572105eb21c484008c50018014377cd6e020de020fc777e977ef370dd470a58e3a71d34720e80481106c83d7103608190
c1060006f7d3abfa557bd1bea5f531a96046890802a57692521b9ce8bf8d5673d1de5bf9e9678d9db208203a86066fa01020bfadd67bd1fedbfde85204ed8a00
20a13ddf8fcb7fe3c71ed7720000d0c5f8101e5077cb2f489bd1181d4308cb6019f6e13fdbf3f88770a2fbc33fa978e3873b83080860f9fee54fdc7a088760a3
ff084f8a7ce18fcc0f186c50710b00d1b6c5171850ee8e6cd171c50a10466c177c50710a1e91b5f511183f1877e3afdcf3f1d79e9af1ef4f9207980055461750
41458e443175441a5094421544413583ffc4384afbefbf5d76f7dffe7cfde78f500b4cd8b18f0631b4c617185082084c8051510c068080313502089090100612
82b752b0190800031038680806101088a80684108061250a0bb31008c1808e1828d1f18082282812328486285252a119083030b2720c08302052120c80003823
33848638385373888a383893b38c0e38a012a4783137035234808c034854f52688484294b4802c4848a1f48e89415815621284581155758418585821b58a8401
58d5201e80687716468582375866868685116836c684816858e6078e8b58582747888558586787828f4848a7c78c894848e708868348372848818c2727688887
822717a8c88d881717e809823c070000d8115465194226ce5c2110010013b0c9911c8003c208f87a59d89ab5da2328e5f0b12b40234c3310e9e15264199766e5
88b5c4f8bd20015184703181098a0a0054ed08f6a079250c0b00c3001db117c2c00861e57891a08ca3222d88b32a44fa18fa9301858fe16700018009edc490cd
00306e370e1004015aa78818087b0b0083a0f8c209fe1088d20d90e464110c57035e00e098c228fad886b30a4861e254b1670e4ec6b6894015b05eb629863b51
a66ca554c4dc00b6b92b121c5825286c500110088d2e6688838c69e1863292445287007710e42340c7ce30c1798b631048a2ee06c9d58834127e848fe071d5e8
39cc1419bbc207f5bcc1b919245377860871145407af8061a9b2da68b44dc480b170fcd90972db85798759a01a2c1a0aac90c18a1438d587a4d04225614e0ec4
7b4eac708e1e83083258f08740428a1692a62aa84c8148020200312402f0524e10a30107422103e8344b2c40c43e81543064640af077c50a0306091c5b471c52
4c2ed0f4278cec01c492870c2204258045410031022832f832f88c89a4b18610228d160c68056810ce0b80c4164844023860c321f1aa007c0a8b884342265345
01501c8c60210e1d737f2ab00a84e0e00029b0703d1825432080410b42a32e84088434604201511b3d14410ae0004b3452cd2fbce8ca90d2453730423b5cc6cb
24714b0560541578dd88ac20821bf05930084c055471491951491e5017c70c80011572001494a88bdc23c230212024588d86e810812192aa26200e479cc24919
51031850064be8534501a08805d400cd04e1030f527f107c604e64620237d09715409540c893052401a42ce446c80050480183240b18ee030030ce35f180097c
e34b24e2871cc1992a788585d8b24b0450fe3f2441c860834a02a00a0554888d30a14e05d2671868d30af4004c39400508f40f4418b0c222521427f4374e58a1
880c025217a0d40880030a11a1b02010104cc411c02f922b0b2406c768a1482060c03a20ca82d4b28322a198099468c0b881071a61822240dd06582005268006
2e60068b5882450432e607c8d2834421ce0d921c2c240a4d9c90020692502da6700a48c3461902d325e0dd00b4334a2033d022d8c10544c3c13013f41580a90b
6cd0c906b11c30dc57455c430d2a92e517145acb0ce1042620500400408ae4130e07f2ca1254080a7ca14b0161100a286208b4814a01a24b28e87b0774830a2a
53a9062c9b469850842db28d28e4b602148242249124a5c42647ac930b2b00b40ac85800b402012581b1311088088892cc1371aa8520d0b2691e4c3214881871
44c05348f1c881005011c20488b4071f68c005812040d3c010405389013c208c403238964812702148080a1192c81818002d844880e30c0e0c32020740534a88
027012080c3080e9844c81854534a0412f116012d3050000f6401368934a04204092cd382043c215701200000600c2890ed893e0826c01854a38c04463140c23
c68e2650b148097852298c446177ce000004450fa853080d0a00a18d1a38c18a032c2063461ac091a6070426280a0c80828c0b28114708687043c506f0350a1b
58f34b8d10d104410c00c2a4832dc12b08021820461114b15b801880530415a831860f6c9164052460d0480538e14b0402d144c31bc852010170f1db4d0050f5
41030443a2051ea024410218436b884420960810900a4100105191840c70810c012091e18960b013c826f0e582150c613f0d047001811318b02e0940b1b7c40c
6061801634c18b0d104061c213b0c2698d39912d0126a8428007200320840010020f1d4093a90c4410c1c22e90058712149fa806201054c8044852620d64e1fa
8b3a1000041118b0a20800612489022080208b087120040400802c12004077061480260c034080020460a0de8a0018b30d12283082820e10d383082003008c48
e1190134c820810b6423118c048171491c3861a2082c5070003a30e6240314c08d0332d06508147801040244b1b2852aa0a66c0a14e3e8820221254801187009
096411da481ca8650a0448324f023a00264408c0634a8a306154482ae02343071092550718e162000f60d38185186100050260c3e41524e0c980243025011c40
136781284120c0348824e90850e23c0316c0914d162812c8802450108b02e0770a032431b60a2080344f048153200e68a0d30628d012060744f06d072ed0878a
2d48c0a3860431304c1c58a2890554425c0c1c1076c40828a2660e0411620100a8b3a51564022d0a08c015c40318f0a681241057ca1050a5c8005c704f8f2491
94890a6801cc804470518a10d8b183136092e482120090c21160c0848138112ac93c8007821d0ca1db042a2084040200d08304042039443818c6441d44726d86
06c0c74c1618332d884c10804402c8a7c10c3c903f0c221160850958a2a00220a0540f30d80206113062a182088094cb0420314d03501086003ca825ca060891
54061a41820409b331ac062060320d0800206c081c82c30a1810150a0348f0c60360406a4c0658007a153486100c1810c4821a80522a0d0840558726d8e1011c
5811cf090661c004431860a98c5051538a3c40518e0a20c18b002e80e44602f032c1866800e843002085ea5b1000f0081410c6c812c011808424618749047841
461b54d273821ea0c10e156043a7091460138734e820e3141cc1a9872a00a44f1690a1630220d126420228c3821b3c9097850cd247801840c000030401a1413a
28822f053852600922d09240493e60c6845c516e5a1ae83840024cc3b2100a71610c1ee043cd0e3c51b4850c88b64d140830390714e004ce0b30d0c002708143
0826c881240958b0810189d060cf1a18110a8b48402a470008408e1b14c1e9071a80e28d16f8d181002450af0d38d831bd2d04ca0b8400e000001500d00a8914
70d3837b1894060210911a8e08e0e70115a0706583b815008b1820256d050453e7081ac142060b28c22e660091c54016d012a5065012b4872840a0c50060e0a0
8d7091684e0480030e192093a08a1021f2820e38f3cd021000204600c884ca0d3081dc86140024848e285303066c001a083ab80588074c418104069007490820
00428650b025091098844f0a506208041cf116051308b083065881c6403400f008091c10580930c8e20c1b4850400cabb08a822ae8b3a40a44f3e88538e051c3
0b60d283884c9199882480020a136091540810a186c505a0a2888104f0200602d08089112850d68c2a4023461c0030c98c00c4c41e0e02504006ca08064040ce
5306073010ae0a0804903031230204a0e024ce0c0930a06b2e050f00800b04010c10106442010900707b4c0901a0352c740d09f0d00419020290108b2e0000a0
e0712406015080462e0b4940a0ce8b030130706cb2010b4060c9c70c01f01031890007a3e003bf05016040223d0f0a20b098e000053020e72341035080284c00
0000b05e7a0909c060d7166002f0d0c3620a009070430a050d20806e843c0d9010b8cd000580b0067b090ca0701e270f0180d0b3060a0820b071600b0810b6a8
8d060460d051890a46d0807143064ce09094a00603c0e022fc00002090ffe70407b0d0e5e10b0e60b0d7e9050b1060b8d20d0db0c0296d0d0ce070a65d580910
30f9340443f00094e1040e209060140804505009e40c06b090044c030f00b07b120500b0e012092106d0a013d20401608018a80e0a5000609c00020010c30c0c
0580c000d4000570601539080c9040bc580d0d2090bd650a0110000d8e0302006043370b054020663c0a0180409e73090700508b09090ad0b038c90b0ac000c3
ed0d0c808097390902f02091f60a0f50d0f23b0b092050ae000d03504004000b65408055070e0e80001200050520205008000a47d0b04e0302c020d26d0d0100
202922010a1070d3020f0130208b910708f00003f8000850509b6905097050d9420008c0504b74040320502ab9080310a01b1d0202f0d075000507808021f308
0a5080bc5b0b0970006542090043603a93090850602b12050550e0121203032020281d0204102043160a01a0808b0a0007a0a099a5040160600415020d80808c
__label__
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11161161111111111111111100111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1116d161111111111111111100111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11666d66611661111111110011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1116d165656d56111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
1116d161656666111111111111111111111111111111111111110011111111111111111111111111111111111111111111111111111111111111111111111111
11166d616566dd111111111111111111111111111111111111110011111111111111111111111111111111111111111111111111111111111111111111111111
11116161611666111111111111111111111111111111111111000111111111111111111111111111111111111111111111111111111111111111111111111111
111111111111111111111111111111111111111111111111100001111111111111111111111111110011111111111111111155dddd5511111111111111111111
1111175111111111111d61111111111111111111111111d11011111111111d61111111111111111001111111111111111dd6666666666dd11111111111111111
1111175111111111111d611111111111111111111111116111111111111115d11111111111111100011111111111111d6666666666666666d511111111111111
1111175111111166111d66651111dd511151d111661115651115dd1111155111115dd111166610011111111111111d66666666666666666666d1111111111111
1111175111115655611d65d611166665116661565561166651d666d116d615611d666d1165566111111111111115666666666666666666666666d11111111111
1111175111115111651d611d61561156117d111511651161116111611661156116111611611511111111111111d66666666666666666666666666d1111111111
1111175111111666651d6111716d11161171111666651161156111651651156156666651d6661111111111111d6666666666666666666666666666d111111111
1111175111116d11651d6111616d11161161116d11651161156111611651156116111111111d611111111111d666666666666666666666666666666d11111111
1111175111116115651d6116d156116d11611165116d1161116115611651156116511611611561111111111566666666666666666666666666666666d1111111
111117666661d6666d1d666d111566d111611156666d1166d15666111651156111666d11d6661111111111166666666666666666666666666666666665111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111d66666666666666666666666666666666666111111
11111111111111111111111111111111111111111111111111111111111111111111111111561111111115666666666666666666666666666666666666d11111
11111111111111111111111111111111111111111111111111111111111111111111111111651111111116666665d666666666665d6666666666666666111111
11133311111111111111111111111111111111111111113333311111111111111111156616661111111156666661566666666666156666666666666d51111111
11113bbbbb333111111111111111111111111111111133bbbb311111111111111111161d616111111111d666666156666666666d0066666666666d1111111111
111113bbbbbbbbb3111111111111111111111111111113bbb31111111111111111111611616111111111d666666016666666666500d66666665dd11111111111
111111bbbbbbbbbb311111111111111111111111111113bbb1111111111111111111166651611111111111d666500666666666d0001666666605611111111111
111111bbbb33bbbbb3111111111111111111111111111bbbb113311111111111111111d111d11111111111111100056666666650000d66666500dd1111111111
111111bbbb11bbbbbb111113b3311111113b331113311bbbb33b3111133b3311133bb313b311311111111111110001666666d1d0000d1666d000161111111111
111111bbbb11bbbbbb1113bbbbbb31113bbbbbb333bbbbbbbbbb1113bbbbbb31113bb3bbbbb3311111111111110000666666d010000106660000056111111111
111111bbbb11bbbbbb313bb31bbbb111bbbbbbbb33bbbbbbbbbb111bb313bbb3113bbbbbbbbb111111111111100000566666d0000000066d00000166d1111111
1111113bbb11bbbbbb31bb111bbbb313bbbbbbbb31bbbbbb3113113b3113bbbb113bbbbbbbbb111111111111100000166666d000000001110000056666ddd111
1111113bbb13bbbbbb33bb113bbbbb1bbbbbbb1131b1bbbb311111bb311bbbbb31bbbbbbbbb311111111115dd0000006d1d1d000000000000000056666666111
1111113bb33bbbbbbb13bbbbbbbbbb3bbbbbb3111111bbbb311111bbbbbbbbbb31bbbb3113b311111115d66610000005d0101000000000000000056666666111
1111113bbbbbbbbbbb13bbbbbbbbbb3bbbbbb3111111bbbb311111bbbbbbbbbb31bbb31111311111111d6666d0000016d000000000000000000005666666d111
1111111bbbbbbbbbb313bbbbbbbbbb1bbbbbbb33b111bbbb311331bbbbbbbbbb31bbb311111111111115666661000056d000000000000000000005666666d111
1111111bbbbbbbbbb111bbbbbbbbb31bbbbbbbbbb111bbbbb33bb1bbbbbbbbbb11bbb311111111111111666661000056d0000000000000000000056666665111
1111111bbbbbbbbb11113bbbbbbbb113bbbbbbbb3111bbbbbbbbb13bbbbbbbb311bbb3111111111111116666d000000dd0000000000000000000056666661111
1111113bbbbbbb31111113bbbbbb11113bbbbbb311113bbbbbb33113bbbbbb3111bbbb11111111111111d66d00000000100000000000000000000566666d1111
11113bbb3333111111111113bb311110033bb3111111113bb311131113bb311113b3333111111111111116660000000000000000000000000000056666651111
1111111111111111111111111111100011111111111111111111111111111111111111111111111111111d661000000000000000000000000000056666611111
11111111111113bbbbb3131111111001111111111111111111111111111111111111111111111111111115661000000000000000000000000000056666511111
111111111111bbbbbbbbbb1111100101111111111111111111111111111111111111111111111111111111d6100000000000000000000000000005666d111111
11111111111bbbbbbbbbbb3111111111111111111111111111111111111111111111111111111111111111161000000000000000000000000000056661111111
11111111113bbbbbbb31333111111111111111111111111111111111111111111111111111111111111111151000000000000000000000000000056651111111
1111111111bbbbbbb311113113bb313bb313113bbbbb3111113bbbbb31113bb33bbbb11111111111111111110000000000000000000000000000056d11111111
1111111113bbbbbbb311111113bb3bbbbbb11bb33bbbb3111bb31bbbb3113bb3bbbbbb111111111111111111000000000000000000000000000005d111111111
1111111113bbbbbbb311111113bbbbbbbbb1bb311bbbbb113b311bbbbb113bbbb3bbbb3111111111111111110000000000000000000000000000011111111111
1111111113bbbbbbbb31133313bbbbbbbb31bb333bbbbb11bb333bbbbb313bbb11bbbbb111111111111111110000000000000000000000000000001111111111
1111111113bbbbbbbbbbbbb313bbb3113b33bbbbbbbbbb13bbbbbbbbbb113bb113bbbbb111111111111111110000000000000000000000000000001111111111
1111111111bbbbbbbbbbbbb11bbb31111113bbbbbbbbb113bbbbbbbbb1113bbb3bbbbbb111111111111111100000000000000000000000000000001111111111
11111111113bbbbbbbbbbbb11bbb31111111bbbbb3331331bbbbb33311313bbbbbbbbb3111111111111110000000000000000000000000000000001111111111
111111111113bbbbbbbbb1311bbb311111113bbbbb333bb13bbbbb333bb13bbbbbbbbb1111111111111000000000000000000000000000000000000111111111
1111111111113bbbbbb311111bbbb311111113bbbbbbb33113bbbbbbb3313b33bbbbb11111111111100000000000000000000000000000000000000001111111
1111111111111133331111113333331111111113333311111113333311113b311333111111111100000000000000000000000000000000000000000000011111
111111111111111111111111111111111111111111111111111111111111bbb11111111111110000000000000000000000000000000000000000000000000111
111111111111111111111111111111111111111111111111111111111111bbb11111111111000000000000000000000000000000000000000000000000000011
111111111111111111111111111111111111111111111111111111111111bbb31111111100000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111333331111110000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111110111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

__map__
2040000000a229f0700507777220b0020de0b810200205b2244010030256be8060010b7c5df0300a01100ba0b00000f33b90200b0aa47650200d0a6d999040028153d1104009058d8d4020010d8d8c20f0030594c070900e0d3243809000090a0cc0100906ca7570d03502d2b190c00801258030b0000a106ca0100509d88a20
e00e0f041da0e00a013599504005018909901003048df880f0080d740c50e10200933a70000b03800fd010000d9015e0e0070653cff0400f09a601609005010101c0700d0ed68ab050030145649060020c3ac480600b0e8f6057467507b82330200c0d7448b0700507b3a640200602edc420100204122210900103e8d8b09009
0c55cc6010020b33244050040d4e7d70200a0110a539a00808a2baa0c00807d20910f04707abc360b0010018aac0500128a1f110100901111510c00004110050c00500be0460600b013a967090080e0fc53080030276e650a00c09d064a0500e04cf8a10a00902254250700004a8dc80900b0b4a98d0100501e63ed010030550
7ed0b6070e2a84e0a00109001480200e00e2021040020c3c400030010e14cfe0c00f6944cf4090070a4b18f0100302a67919a0060aa85ca0300103525280a00705d253a0108a00406130b03804411010e00e07c2d400500803610c003007035dee80300c01072290300405e2971000000b9c5e8060020209ccf0900f017e0000
30010a042910c00d004642e0200709946410000906241830900004eed59130090b81d030500700e53250d00c065d6240b0070e738640100d084d00b0300a66855d10e00308949c50c002012573a0e00e0ba4a6c0c00c0cecd40060030e36e0d0100053824c00400904cd8740c0050b8ee18030640aec8cd0000b0555a7a0d006
05a158c0700308cfa760400f0fd319d0f00f0f0296f0c005035a9990a0060a15278011060b6d042080020a6bb24000040e0d22f02000032c1b30300201111360900a093c7de070050cbfa860500603080230a009028048c230040668332060070c3a85a00007069cf440300003421d70400b03b4c000a0690c8b3b80400508a6
c8405008074b2270500c0606f2a0c30c0784cc40a0010971acf0700a035047c0d000073610a0000c02748070900b0d8d94a0100402433d50901a066fe5d0d00305a2959060050a45d50040090833ba90900108811d204000059993b0d00a0ed1db80a00504afc480f008088b7850300e07591210600f0bad2b8010040af9f7c2
e0070cd5ecf07a07016cd33050060a40a440000c0003340050060d92a210d00806119010400895828df0800204c826907009091944d00002088c72f0f00401ff4fc010030ad7ea10200e3981fc00200b0eb92ae0700a036a0580800103123a40300305615c40500e00042a00101c03233a30200602d2663030060a950460300a
010432b0500705255a80600602f26550c00509116da930050d64ae50900a09cbc490700603b69ac070080c8ddef000040adf150030040b0074c0000c0cc308f0c00b0ccdebf0e00c0a7c608ca00e06cda08000c60ce9602000080447b1e0900a0dd96ab0e00f016817c0500108e25dc09008099da8f010050d487920d00d01d8
c980000b088551a040050b2846b0c00b01edf410500a060001e0f30c02419e001001096c5100200c0856db07300b06f2856010000a0ee74052000ca40260400e026203503006094ba840300a00622a10460407a37950a004026acdd02009046ef570500a0577be1090020be67340a005049904b0000a00241e1020010c803501
7000058ba680800208c96a4120070c699c90400004d8244000050a8c4390a00808cf3340200a0ceda610800506668260000705920f0040030a2d3530b0030c88c420280f04104c3050a3a29431c0100e0d6cc4c0b003026e2ae0f0080476daa0600a00f78d4090000076c080600c0d046e509000095a6660a00f0a5aa5504005
035213a0000d08218360b00c3c790db01005016a32c0000109a8f400f00b0889108010040fcfe3f0f0070614238060070601ed50400b097710a0a00402caa25020090983da20d0040d93a850e006046c0df0700d04a0294080090a8711106002cb102240e00305e914a0000f0a229c10030d09248980400309ff3920700609da
b9a0c00103aff550b00f0aba3dd0c00901cc9cc0400304b4cc8040570485d838d0010635ca10100b0b9c4e20000107f0e6f6f0080cde6c02000304052320300254380fd0500933d51b80400508f1b3c0900029278e00500105286a3080d903280340200204030150e0020b006e00c09901a94840a0000620229020070c3e7a50
300800de302080000c843c30c0520b121180f0cb080c78e010090434d220e006081cc2708007064e097020030d8c7bf0200c0e58b830f0090d2718a0100e0005cca03008081424d0700d009c0180a00400d2d3e0107c089c15709007023058b022060803e8b0800c0e103900700800299140e00c007343d0804b00dce7002007
042c712040069a867c60200006c018c01001097cef92900c03048540c00805ccf57c800a0c74f8bcd0080c5ee840c0060cc00e1060040e6dd4d0d003055604a0400b06a762505008c5226cf0c00c08747c40c000018d955050060e9d12d0400f0ede7ac03a0200a6132030040ab005802005047bb530d00a0e55f9909003011f
212020030b692200400a0ab3a5b0700b09dc3090601c076f1f30a0020516e910c70808aeea10000c018df63070040fa54210000c02110b30f00b0085e2c0e006037380f0c0096fc46120c0090e1ee6509005022a0f2070010a9cfc00900408448800b0080478f3f0100a05383d00b00f0b4eacf0800e0a38db70500305f67490
50030c67bae03005052b80f030090ab41e20000e3f4c4d309000014a25a080000d586c60d00cef645b30450200bcc9e0e00801ea873070070e4fb2c0600dc3493b80d00309129370300d02f828c590040543c4c0f00c045e8ce0800707094980200502cece40600c0e5b8250600f02445260e06b0d9b5850b0020f0e8280800e
09da2f20e005032365d0400d070b3d5030000275b3b03d050e10a4705009003a45a1800b0078d020900c0b0cb9801005002cf000f0000d0589500007005a88b0700503f127d0f00ea6d442b050000229a8a0a00405e42920300407417240b00a04a34da0a0070e7c1b00a003072115b6700e064605066003027c838020080d2e
ac20c00e04cccb7000090ec7dd50e00403c02b40600f2c68380080020464aa409409091ac1b0e0020629eb40100000683810c00c0639db50200305edb6a02004933436a0e0000d2007b00001099251e0e00d19148d501000ba1f88a0a00509f23fe0da00013f7d80c000024092303004002240d00005042b8b09e0010a306aa0
300a0eaff6a0a50006184940b00602ab7640700c0a47a240e00c05dd9200800a0c060440f0060a5514e0000107e928209f06024804683a00000d024213642e601802813b040471f0d4820008003620680e0150001a08080493201846001c0ca409904480800505093cc82240900a017633d877400000c1100a0060204402a009
018c31406080600201261294aaa08802811c044649e08c00201c03c915684042a00a001d0660a580c80641360a366a508881400807792840de41f005022115b45a40600503883c7e2d90a402801401dd04a052812002026a093454c0c803c13502a800f0a882230000901030ae09b0088003030c10c08003df2804b20f80b003
801802c413a070c55004000215f0d4a0a8060114065072c02801400905b4324056815005801e13ec6e40280400120e4a06404801e01a060414608641800003241d3cf700780480330bc20580e080401306bc03f0a040000081458b3135a0980241120b320f5028024001077617a89441d009a21a11e4eb20f001002e0de025b0
800200130ac42c50c001f000837706d80a407801003904ba4220f80c401501823748a640f009826900acc8400000cb18054017b00401e00d0114265042900003bd61083472e03007d50c0cbc3010c0018003004709901c807003005e011024c08003c00b0c1a18100803e00e00e302404400c00f801f08387000a84a00050aa0
74308800200f010e0e409c405005024414748f402107c151058eb7d0ec802011043c11b07c80f0080204170ce2201006000209d05da078808003010821b85e41e002017103f8bbc0580401070e406e80f0820003031a37d05e80b00b013c07004c409800002f1581a9085245d12a0058f603954e381645b22c9667d1448b625a
16d5b6b8b645d12d8b6f5c84e562381745ba2ed677d1c48b625e17f5beb8f645d12f8c7f60040563381846c2301687d1448c63621815c6b83646d1318c8f64842563381946ca325697d1c48c63661935ceb87646d1338d9f68044563381a46d23496a7d1448d636a1a55d6b8b646d1358daf6c846563381b46da36d6b7d1c48d
636e1b75deb8f646d1378ebf70048563381c47e23816c7d1448e63721c95e6b83647d1398ecf7484a563381d47ea3a56d7d1c48e63761db5eeb87647d13b8cdfdcc18680480447f10890e3512c8f637a1ecff468ae47513d8fed7b84e523f81ec7f93ebaf1d1a48f637e1feffc58ee47513f90fd7f040524f91fc70140fa01d2
249064822b0ed0f806006a348ddd84144323282148d0335216f2f48c2386213bcea8724872338c1e88d43323282248cc329226f2b48c238a222bcaa8b24872328c2e8c942323282348c831d236f2748c238e231bc6a8f24872318c3e90541323282449c4301246f2348c2392240bc2a8324972308c4e94140323282549c02f52
56f2f48b229625fbbea87249722f8b5e98d4f322282649bc2e9266f2b48b229a26ebbaa8b249722e8b6e9c94e322282749b82dd276f2748b229e27dbb6a8f249722d8b7ea054d32228284ab42c1286f2348b22a221cb000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
636b2c300000000000000000000000000000000000000000000000000000000000000000000000008001856635517295162c0033811624103300623004733403300032c0033850332003300622d5062917622506
2c3078303851626103300623004731003300032c003385033000330062251762451723117255262e1623004724136300032c0033850330003300623004731003300032c8700a6052713638162300472212630003
30302c9030003300623004731003300032c436261471f1362c506275172c0033851624103300623004733003300032c00338103310033006233136381623004723526300032c0033851332003300623004731403
00000000142612c526365262c5172c0033811624103300623004730003300032c0033812333003300620000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000

