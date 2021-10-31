pico-8 cartridge // http://www.pico-8.com
version 33
__lua__
--the laboratories of dr. creep
-- by gpi 2021

cartdata("3011c98f_908a_471f_ab56_3509c90b9850_creep")
poke(0x5f36,16)--enable high memory

function tape_rewind()
  block_load ("levels",true)
  tape_pos,tape_end,tape_bit=0xbd00,0x0000,0x8000
end
function tape_rewind_save()
  tape_pos,tape_end,tape_bit=0x4300,0x4400,0x8000
end

function tape_over()
  return tape_pos>=tape_end
end
--[[
function tape_backbit(x)
  for i=1,x do
    tape_bit<<=1
    if (tape_bit>128) tape_bit=1 tape_pos-=1
  end
end
--]]

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
     ret ..= chr(tape_bits(5)+96)
  end
  return ret
end

function msets(x,y,w,h,s)
  for dx=0,w-1 do
    for dy=0,h-1 do
      mset(x+dx,y+dy,s+dx+dy*16)
    end
  end
end

function tape_list_laby()
  local list,save = {},laby
  tape_rewind()
  while tape_read_laby() do
    add(list,laby.name)
  end
  laby=save
  return list
end

function tape_find_laby(name)
  tape_rewind()
  while tape_read_laby() do
    if (laby.name == name) return true
  end
  new_laby()
  return false
end

function get_save_info()
  if (dget"0"==0) return ""
  memcpy(0x4300,0x5e00,0x100)
  tape_rewind_save()
  return tape_getstr().." ("..playertxt[tape_bits"1"+1]..")"
end

function game_save()
  if @0x4300!=0 then 
    memcpy(0x5e00,0x4300,0x100)
    print_msg "game saved"
  else
    print_msg "can't save, too complex lab"
  end
end

_saveitems=split("con,dir,3,lma,3,1,swi,3,1,trp,5,1,tmc,1,3")
 

function game_load()
  memcpy(0x4300,0x5e00,0x100)

  tape_rewind_save()
  tape_find_laby(tape_getstr())
  tape_rewind_save()
  tape_getstr()
  num_player=tape_bits"1"+1
  laby_init()

  for p in all(player) do
    p.time=tape_bits"8" <<8 |
           tape_bits"8" |
           tape_bits"8" >>8 |
           tape_bits"8" >>16
    p.x=tape_bits"4" *8+4
    p.y=tape_bits"4" *8
    p.r=tape_bits"5"
    p.state=p.r==0 and "quit" or "alive"
  end
  for i=1,tape_bits"4" do
    add(player.key,tape_bits"3")
  end
  for d in all(laby.door) do
    d.open= tape_bits"1"==1
  end
  
  for r in all(laby.room) do
    r.visited=tape_bits"1"==1
    for i=1,#_saveitems,3 do
      for c in all(r[_saveitems[i]]) do
        c[_saveitems[i+1]]=tape_bits(_saveitems[i+2])
      end
    end

    --key
    for k in all(r.key) do
      k.picked=tape_bits"1"==1
    end
   
    --tmd
    --mum
    function load_mon(what)
      for m in all(r[what]) do
        m.x,m.y=tape_bits"4" *8+4,tape_bits"4" *8
  if (what=="frk") m.dir=tape_bits "3"
        m.state="alive"
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
  tape_setstr(laby.name)

  tape_bits(1,num_player-1)

  for p in all(player) do
    tape_bits(8,p.time>>8 )
    tape_bits(8,p.time    )
    tape_bits(8,p.time<<8 )
    tape_bits(8,p.time<<16)
    tape_bits(4,p.x\8)
    tape_bits(4,p.y\8)
    if p.state!="quit" then
      tape_bits(5,p.r)
    else
      tape_bits(5,0)
    end
    --state is always new
  end  
  tape_bits(4,#player.key)
  for k in all(player.key) do
    tape_bits(3,k)-- 0=8! correct!
  end
  for d in all(laby.door) do
    tape_bits(1,d.open and 1 or 0)
  end
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
  --printh(tape_end-tape_pos)
  if (tape_over()) poke(0x4300,0)
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

function str2(x)
  x="00"..flr(x)
  return sub(x,#x-1,#x)
end

function timestr(t)
  return t\60 ..":".. str2(t%60)
end

function printtutorial(i)
  local y,c=16,10
  for l in all(split(tutorialtxt[i],"\n")) do
    w=print(l,0,-10)
    print(l,64-w\2 + (i==8 and 16 or 0),y,c)
    y+=c==10 and 10 or 6
    c=9
  end
end

forcecolor=split("7,7,15,15,10,10,15,15")
colors=split("11,3,6,5,8,2,14,2,10,9,12,13,4,2,0,1,13,1,15,14,9,4,3,5,5,0")

playertxt={"one player","two players"}
lad_pol=split("lad,pol")

trp_spr=split("0x30,0x31,0x31,0x32,0x32,0x33,0x33,0x02")
frc_spr=split("0x7a,0x79,0x78,0x77,0x76,0x5f,0x5e,0x5d,0x5c")

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
operate the gun by
standing in front of
the control and using
⬆️ and ⬇️ to aim and
❎ to fire. the gun
fires automatically
when it is at the same
level as a player.
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
-->8
function new_room()
  local a= {visited=false}
  for x in all(split "way,lad,pol,bel,loc,key,con,lma,swi,frc,trp,tmc,tmd,mum,frk,gun") do
    a[x]={}
  end
  return a
end

function new_laby()
  laby={
    name = "unnamed",
    room = {},
    door = {},
    player =split("1,1,1,2,1")
  }
end

function _readtable(lin)
  local c={}
  for i=1,#lin do
    add(c,tape_bits(sub(lin,i,i)))
  end
  return c
end

function _read(what,lin)
  for a=1,tape_bits(4) do
    add(room[what],_readtable(lin))
  end
end

function tape_read_laby()
  local n,c,r= tape_getstr()
  if n=="" then
    laby=nil
    return false
  end
  function _correct(what)
    for w in all(room[what]) do
      if (w[1]==0) w[1]=16
    end
  end
  function _box(data)
    local s=split(data)
    for a in all(room[s[1]]) do
   a[s[2]]={
        a[s[3]]*8+s[5],
        a[s[4]]*8+s[6],
        a[s[3]]*8+s[7],
        a[s[4]]*8+s[8]
      }
    end
  end
  
  local readmatrix=split("1,bel,444,2,loc,4443,3,key,344,4,con,24444,5,lma,441,5,swi,4418,6,frc,4444,7,trp,44441,8,tmc,344,8,tmd,044,9,mum,4444,10,frk,441,11,gun,4444441",",",false)
  
  new_laby()
  laby.name = n
  for ii=1,tape_bits"5" do
    room=new_room()
    room.col=tape_bits"4"
    room.x=tape_bits"4"
    room.y=tape_bits"4"
    room.w=tape_bits"3"
    room.h=tape_bits"3"
    _read("way","444")
    _correct("way")
    for p in all(room.way) do
     p.type="way"
     p.box={
      p[2]*8+2,
      p[3]*8,
      (p[2]+p[1]-1)*8+7,
      p[3]*8+7
      }
    end
    _read("lad","444")
    _correct("lad")
    for p in all(room.lad) do
     p.type="lad"
     p.box={
       p[2]*8+2,
       p[3]*8,
       p[2]*8+5,
       (p[3]+p[1]-1)*8
       }
    end
    _read("pol","444")
    _correct("pol")
    for p in all(room.pol) do
     p.type="pol"
     p.box={
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
 
    _box "bel,switchbox,2,3,2,3,5,10"
    _box "loc,switchbox,2,3,2,3,5,10"
    _box "key,box,2,3,2,3,5,10"
    _box "con,switchbox,4,5,2,3,5,10"
 _box "con,movebox,2,3,2,0,12,7"
    for p in all(room.con) do
      p.anim=0
   p.dir=0
      if (p[1]==2) p.dir=1
      if (p[1]==3) p.dir=3      
    end
 _box "lma,deathbox,1,2,1,0,6,15"
    _box "swi,switchbox,1,2,2,3,5,10"
    _box "frc,switchbox,3,4,2,3,5,10"
 _box "frc,frcbox,1,2,2,0,5,15"
    _box "trp,switchbox,3,4,2,3,5,10"
 _box "trp,deathbox,1,2,2,0,5,7"
    for p in all(room.trp) do
      p.intouch=false
    end
 _box "tmc,box,2,3,3,1,15,12"
    for d in all(room.tmd) do
      d.x=d[2]*8+4
      d.y=d[3]*8+8
      d.obj=findway(d.x,d.y)
    end
 _box "mum,switchbox,3,4,2,3,5,10"
    for m in all(room.mum) do
      m.sx=m[1]*8+8
      m.sy=m[2]*8+8
      m.anim=0
      m.x=0
      m.cx=0
      m.y=0
      m.obj=findway(m.sx,m.sy)
      m.state="home"
      m.type="mum"
      m.speed=0.4
      m.killer=true
      setbox(m)
    end
    _box "frk,homebox,1,2,3,2,4,15"
    for f in all(room.frk) do
      f.sx=f[1]*8+4+(f[3]==1 and 1 or -3)
      f.sy=f[2]*8+8
      f.anim=0
      f.x=0
      f.cx=0
      f.y=0
      f.dir=f[3]
      f.state="home"
      f.type="frk"
      f.speed=0.50
      f.killer=true
      setbox(f)
    end
    _correct("gun")
    _box "gun,switchbox,5,6,2,3,5,10"
    for a in all(room.gun) do
      a.x=a[2]*8
      a.y=(a[3]+a[4])*8
      a.x2=a.x + (a[7]==0 and 8 or -8)
      a.min=a[3]*8
      a.max=(a[3]+a[1]-1)*8
      a.speed=0.25
      a.shootbox={}
      a.shootspeed=(a[7]==0 and 4 or -4)
    end
    --
    add(laby.room,room)
  end
  --door
  repeat
    c=tape_bits"5"
    if (c==0) break
    local d={c,tape_bits"4",tape_bits"4",tape_bits"5"}
    if d[1]==d[4] then
      d[5]=d[2]
      d[6]=d[3]
    else
      d[5]=tape_bits"4"
      d[6]=tape_bits"4"
    end
    d.open=false
    add(laby.door,d)
  until false
  --playerdata
  laby.player=_readtable "54444"
  return true
end

function draw_lma(m)
 local y=m[2]-1
 
 while fget(mget(m[1],y),7) and y>0 do
   mset(m[1],y,m[3]==1 and 0x58 or 0x34)
   y-=1
 end
end

function findway(x,y)
 for s in all(split("way,lad,pol")) do
   for w in all(room[s]) do
     if (w.box[1]<=x and x<=w.box[3] and w.box[2]<=y and y<=w.box[4]) return w
   end
 end
 --return nil
end

function laby_draw_map()
  memset(0x2000,0,0x1000)
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
      if (mget(p[2],i)>=1 and mget(p[2],i)<=3) mset(16+p[2],i,0x14)
      mset(p[2],i,0x04)
    end
    mset(16+p[2],p[3]+p[1]-1,0x24)
  end
  --pol
  for p in all(room.pol) do
    for i=p[3],p[3]+p[1]-2 do
      if (mget(p[2],i)>=1 and mget(p[2],i)<=3) mset(32+p[2],i,0x15)
      mset(p[2],i,0x05)
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
     d.ox=d[off+1]*8+8
     d.oy=d[off+2]*8+8
     
     d.box={d.ox-4,d.oy-4,d.ox+3,d.oy+3}
     d.sx=d[6-off]*8+8
     d.sy=d[7-off]*8+8
     d.room=d[5-off]

     d.col=d[1]==d[4] and 5 or laby.room[d.room].col
     msets(d[off+1]+16,d[off+2],2,2,d[1]==d[4] and 0x08 or 0x06)
     d.anim=d.open and 8 or 0
     
   end
 end
 --bel
 for b in all(room.bel) do
   b.col=doors[b[1]].col
   --mset(b[2],b[3],0x4f+d.col)
 end
 --loc
 --for l in all(room.loc) do
 --  mset(l[2],l[3],0x59+l[4]-1)
 --end
 --key
 for k in all(room.key) do
   if (not k.picked) mset(k[2],k[3],0x25+k[1])
 end
 --con
 for c in all(room.con) do
   local s=0x4d
   if (c.dir==1) s=0x4e
   if (c.dir==3) s=0x4f
   mset(c[4],c[5],s)
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
   --mset(f[3],f[4],0x2d)
   f.activ=1
   f.delay=0
 end
 --trp
 for t in all(room.trp) do
   --mset(t[1],t[2],0x30+t[5]*3)
   --mset(t[3],t[4],0x3e+t[5])
   t.anim=#trp_spr * t[5]
 end
 --tmc
 for t in all(room.tmc) do
   msets(t[2]+32,t[3],2,2,0x10)
   t.delay=0
   t.tele=0
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
   --mset(g[5],g[6],0x40)
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

-->8
--init & update

function _init()
  state="main_init"
  labylist=tape_list_laby()
  num_player=1
  frc_anim=0
  msg=""
  msg_anim=0
end

function print_msg(x)
  msg=x
  msg_anim=0
end

function _update()
  oldstate=state
  --assert(update[state]!=nil,"unknwon state:"..state)
  update[state]()
end

update={}

function update.main_init()
  save_state_txt=get_save_info()
  state="main"
  block_load "mainback"
  block_load "music"
  music(0)  
end

function update.main()
  menu_text={}
  add(menu_text,playertxt[num_player])
  add(menu_text,save_state_txt)
  add(menu_text,"")
  for i in all(labylist) do
    add(menu_text,i)
  end
  menu_pos=1
  menu_retstate="main_selected"
  menu_title = "the laboratories of dr. creep"
  menu_right = ""
  state="menu"
  for i=1,5 do
    menuitem(i)
  end
end

function laby_init()
  player={key={}}
  for i=1,num_player do
   add(player,{
    dir=0,
    d=nil,
    speed=1,
    type=i,
    state="new",-- initalize to start
    box={0,0,0,0},
    anim=0,
    isplayer=true,
    time=0
   })
  end
  activeplayer=0
  state="room_init"
end

function show_map() 
 if state=="show_map" then
   state=save_state
 else
   save_state,state=state,"show_map" 
 end
end

function update.main_selected()  
  if menu_pos==1 then
    num_player=(num_player % 2)+1
    menu_text[1]=playertxt[num_player]
    state="menu"
  elseif menu_pos==2 and save_state_txt!="" then
    game_load()
  elseif menu_pos>3 then

    tape_find_laby(menu_text[menu_pos])
    laby_init()
  end

  if state!="menu" then
    music(-1)
    block_load "sfx"   
    menuitem(4,"suicide",function()
      for p in all(player) do
        if (p.state=="alive") die_creature(p)
      end
    end)
    menuitem(2,"save",game_save)
    menuitem(3,"load",game_load)
    menuitem(1,"map", show_map)
  end
end
function setbox(t)
 t.box={
  flr(t.x)-2,
  flr(t.y)-3,
  flr(t.x)+1,
  flr(t.y)+4
 }
end

function update.room_init()
  local quitcount=0
  --revive player and quit-fix
  for p in all(player) do
   if p.state=="death" or p.state=="new" then
     p.x=laby.player[p.type*2]*8+4
     p.y=laby.player[p.type*2+1]*8
     p.r=laby.player[1]
     p.door=nil
     if (p.state=="death") p.time+=60*5
     p.state="alive"
   elseif p.door!=nil and p.door[1]==p.door[4] then
     p.state="quit"
     p.door=nil
   end
   if p.state=="quit" then
     if (#player==2) p.r=player[3-p.type].r
     quitcount+=1
   end
  end

  if quitcount==#player then
    state = "laby_exit"
  else
   activeplayer = activeplayer % #player +1
   roomnb = player[activeplayer].r
   room=laby.room[roomnb]
   room.visited=true
   creatures={}
   laby_draw_map()
   state = "room_play"

   --attach player to walkway
   for p in all(player) do
    if p.state!="quit" then
      if p.r == roomnb then
        add(creatures,p)
        setbox(p)
        p.obj=findway(p.x,p.y)
        p.state = p.door==nil and "alive" or "outdoor"
      else
        p.state="nothere"
      end
      p.stateanim=0
    end
   end
 end
 game_checkpoint()
end

function cancross(t)
  if t.obj.type=="way" then
    
    for s in all(lad_pol) do
      for l in all(room[s]) do
        if (box2box(t.box,l.box) and flr(t.x) == l.box[1]+2) return true
      end
    end
    
  else--if t.obj.type=="lad" or t.obj.type=="pol" then
    for w in all(room.way) do
      if (box2box(t.box,w.box) and flr(t.y) == w.box[2]) return true
    end
  end
  return false
end

function domove(t,dx,dy,dir)
  local oldx,oldy,oldobj = t.x,t.y,t.obj
  dir=dir==nil and true or false
  -- change object?!
  if t.obj.type=="way" then
    if dy!=0 then
   for s in all(lad_pol) do
        for l in all(room[s]) do
          if box2box(t.box,l.box) then
            if flr(t.x) < l.box[1]+2 then
              if (dx==0) dx=t.speed
            elseif flr(t.x) > l.box[1]+2 then
              if (dx==0) dx=-t.speed
            else
              t.obj = l
              dx=0
            end
       end
        end
      end
    end
  else--if t.obj.type=="lad" or t.obj.type=="pol" then
    if dx!=0 then
      for w in all(room.way) do
        if box2box(t.box,w.box) then
          if flr(t.y) < w.box[2] then
            if (dy==0) dy=t.speed
          elseif flr(t.y) > w.box[2] then
            if (dy==0) dy=-t.speed
          else
            t.obj = w
            dy=0
          end
        end
      end
    end
  end

  if t.obj.type=="way" then
    t.x=mid(t.obj.box[1],t.x+dx,t.obj.box[3])
    if (dir) t.dir= dx<0 and ⬅️ or ➡️

  elseif t.obj.type=="lad" or t.obj.type=="pol" then
    if (t.obj.type=="pol" and dy<0) dy=0
    t.y=mid(t.obj.box[2],t.y+dy,t.obj.box[4])
    if (dir) t.dir= dy<0 and ⬆️ or ⬇️

  end

  -- collision with other creature?

  local mask= t.type==1 or t.type==2
  for c in all(creatures) do
    if (c==t) mask=true
    c.oldcol=mask and box2box(c.box,t.box)
  end
  for f in all(room.frc) do
    f.oldcol=f.activ<=1 and box2box(f.frcbox,t.box)
  end
  for f in all(room.frk) do
    f.oldhomecol=box2box(f.homebox,t.box)
  end

  setbox(t)

  local doreset=false
  for i,c in pairs(creatures) do
    if c!=t and c.oldcol==false and box2box(c.box,t.box) then
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
    if (f.oldcol==false and f.activ<=1 and box2box(f.frcbox,t.box)) doreset=true
  end
  -- force frankenhome
  for f in all(room.frk) do
    if (f.oldhomecol==false and box2box(f.homebox,t.box)) doreset=true
  end
  -- reset position
  if (doreset) t.x,t.y,t.obj=oldx,oldy,oldobj setbox(t)

  local xx=t.x/8%1
  if (xx>0.4 and xx<0.6) t.cx=t.x
  if (t.obj.type=="pol") t.dir=3

  return t.x-oldx,t.y-oldy
  --printh(t.x .. " " .. t.y .." " ..t.obj.box[1] .." "..t.obj.box[2])
end

function die_creature(c)
 if c.state=="alive" then
  c.state="dying"
  c.stateanim=0
  del(creatures,c)
  sfx(2)
 end
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

function update.room_play()
 local s,dx,dy,con,move,p,y
 -- animations
 laby_draw_anim_map()

 -- players in room?
 s=false
 for p in all(player) do
   if (p.state!="quit" and p.state!="nothere" and p.state!="death") s=true
 end
 if (not s) state="room_init"
 
 -- gun reset control
 for a in all(room.gun) do
  a.ctrl=-1
 end

 -- update player
 for p in all(player) do
  if p.state=="alive" then
   p.time+=0.0333 -- 1/30
   dx,dy,con=0,0,p.type-1
   local btn⬆️,btn⬇️,btn⬅️,btn➡️,btn❎ = btn(⬆️,con),btn(⬇️,con),btn(⬅️,con),btn(➡️,con),btnp(❎,con)
   if (btn⬅️) dx=-p.speed
   if (btn➡️) dx=p.speed
   if (btn⬆️) dy=-p.speed
   if (btn⬇️) dy=p.speed

   if (dx!=0 or dy!=0) update_anim(p,domove(p,dx,dy))

   if (btnp(🅾️,con)) show_map()
   
   --gun
   for a in all(room.gun) do
    if box2box(p.box, a.switchbox) then
     if (btn⬆️) a.ctrl=⬆️
     if (btn⬇️) a.ctrl=⬇️
     if (btn❎) a.ctrl=❎
     if (a.ctrl==-1) a.ctrl=-2
    end
   end
   --transmitter
   for c in all(room.tmc) do
    if box2box(c.box,p.box) then
     if btn⬆️ or btn⬇️ then
      if c.delay<=0 then
       c.delay=20
       c[1]=(c[1] % #room.tmd) +1
       sfx(15+c[1])
      end
     elseif btn❎ then
      p.x,p.y,p.obj=room.tmd[c[1]].x,room.tmd[c[1]].y,room.tmd[c[1]].obj
      setbox(p)
      sfx(24)
      c.tele=15
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
      for i=1,8 do
       s<<=1
       if (a[4]&s!=0) room.lma[i][3]^^=1 draw_lma(room.lma[i])
      end
      sfx(5)
     end
    end
   end

   if btn❎ or btn⬆️ then
    --enter door
    for d in all(doors) do
     if d.open and box2box(d.box, p.box) then
       p.state="indoor"
       p.stateanim=0
       p.door=d
       p.x=d.sx-2+p.type*2
       p.y=d.sy
       p.r=d.room
       del(creatures,p)
     end
    end
   end

   if btn❎ then
    --key
    for k in all(room.key) do
     if not k.picked and box2box(k.box,p.box) then
       add(player.key,k[1])
       k.picked=true
       mset(k[2],k[3],0)
       sfx(28)
     end
    end
    --doorbel
    for b in all(room.bel) do
     if doors[b[1]].open==false and box2box(b.switchbox, p.box) then
      doors[b[1]].open=true
      sfx(27)
     end
    end
    --lock
    for l in all(room.loc) do
     if doors[l[1]].open==false and box2box(l.switchbox, p.box) then
      for k=1,#player.key do
        if player.key[k]==l[4] then
          doors[l[1]].open=true
          sfx(27)
          deli(player.key,k)
          break
        end
      end
     end
    end
    --control conveyer
    for c in all(room.con) do
     if box2box(c.switchbox, p.box) then
      c.dir=(c.dir+1)%4
      s=0x4d
      if (c.dir==1) s=0x4e
      if (c.dir==3) s=0x4f
      mset(c[4],c[5],s)
      sfx(s==0x4d and 3 or 4)
     end
    end

    --control force field
    for a in all(room.frc) do
     if box2box(a.switchbox,p.box) then
      a.activ=#frc_spr
      a.delay=30
      sfx(15)
     end
    end
   end

  end
 end

 -- update gun
 for a in all(room.gun) do
  p = get_near_player(a)
  a.spr=0x40
  if p~=nil then
   y=p.y-3
   if (a.ctrl==⬇️ or (a.ctrl==-1 and y > a.y)) a.y+=a.speed a.spr=0x42
   if (a.ctrl==⬆️ or (a.ctrl==-1 and y < a.y)) a.y-=a.speed a.spr=0x41
   a.y=mid(a.min,a.max,a.y)
  end
  for p in all(player) do
   --y=p.y-3
   if a.shoot==false and (a.ctrl==❎ or (a.ctrl==-1 and p.box[2]<=a.y+3 and a.y+3<=p.box[4])) then
    a.spr=0x43
    a.shootbox={a.x2,a.y+3,a.x2+8,a.y+3}
    a.shoot=true
    sfx(26)
   end
  end
  if a.shoot then
   a.shootbox[1]+=a.shootspeed
   a.shootbox[3]+=a.shootspeed
   for c in all(creatures) do
    if (box2box(c.box,a.shootbox)) die_creature(c) a.shoot=false
   end
   for f in all(room.frc) do
    if (f.activ<=1 and box2box(f.frcbox,a.shootbox)) a.shoot=false
   end

   if (a.shootbox[3]<0 or a.shootbox[1]>128) a.shoot=false
  end
 end
 --update frankenstein
 for a in all(room.frk) do
  if a.state=="home" then
   for p in all(player) do
    if p.state=="alive" and a.sy==flr(p.y) then
     if (a[3]==1 and a.sx<=p.x) or (a[3]==0 and a.sx>=p.x) then
      a.state="alive"
      a.stateanim=0
      a.x=a.sx
      a.cx=a.x
      a.y=a.sy
      a.obj=findway(a.x,a.y)
      setbox(a)
      sfx(14)
     end
    end
   end
  elseif a.state=="alive" then
   if (a.obj==nil) a.obj=findway(a.x,a.y)
   if cancross(a) then
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
     for a=i+1,#move do
      if (move[a][2]<move[i][2]) move[i],move[a] = move[a],move[i]
     end
     a.dir=move[i][1]
     dx,dy=domove(a,dirxy(a.dir,a.speed))
     if (dx!=0 or dy!=0) update_anim(a,dx,dy) break
    end
   end

   dx,dy=domove(a,dirxy(a.dir,a.speed))
   update_anim(a,dx,dy)
   if (dx==0 and dy==0) a.dir=backdir(a.dir)
  end
 end

 -- update mummy
 for a in all(room.mum) do
  if a.state=="home" then
   for p in all(player) do
    if p.state=="alive" and box2box(a.switchbox,p.box) then
     a.state="init"
     a.stateanim=0
     sfx(25)
     msets(a[1]+16,a[2],2,2,0x2e)
    end
   end
  elseif a.state=="init" then
   a.stateanim+=0.5
   if a.stateanim>=8 then
    a.x=a.sx
    a.cx=a.x
    a.y=a.sy
    a.state="alive"
   end
  elseif a.state=="alive" then
   p= get_near_player(a)
   if p~= nil then
    if (flr(p.x)>flr(a.x)) update_anim(a, domove(a,a.speed,0))
    if (flr(p.x)<flr(a.x)) update_anim(a, domove(a,-a.speed,0))
   end
  end
 end
 -- update transmitter chamber
 for a in all(room.tmc) do
  if (a.delay>0) a.delay-=1
 end
 -- update force field
 for a in all(room.frc) do
  if a.activ>1 then
   a.delay-=1
   if a.delay<=0 then
    a.delay=30
    a.activ-=1
    sfx(14-a.activ)
   end
  end
 end
 for c in all(creatures) do
   -- update lighting machine
   for p in all(room.lma) do
     if (p[3]==1 and box2box(p.deathbox,c.box)) die_creature(c)
   end
 -- update conveyor bell
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
   if (box2box(c.box,t.switchbox)) s=true
   if (t.anim<=1 and box2box(t.deathbox,c.box)) die_creature(c)
  end
  if s==true and t.intouch==false then
   t[5]^^=1
   sfx(t[5])
  end
  t.intouch=s
 end
 -- update door
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

  if (btnp "2" and menu_pos>1) _dox(-1)
  if (btnp "3" and menu_pos<#menu_text) _dox(1)

  if (btnp "4" or btnp "5") state=menu_retstate

end

function update.laby_exit()
  for p in all(player) do
    p.x=-32*p.type
    p.y=111
    p.anim=0
    p.state="walk1"
    p.stateanim=0
  end
  state="laby_exit_anim"
  block_load "endscreen"

  for i=1,5 do
    menuitem(i)
  end
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
  if (out == #player) state="main_init"
end

function update.show_map()
  for p=0,1 do
    if (btnp(❎,p) or btnp(🅾️,p)) state=save_state
  end
end

-->8
--draw

function _draw()
  if draw[oldstate]!=nil then
    draw[oldstate]()
  end
  
  if msg!="" then
    local y=0
    if msg_anim<8 then
      y=-8+msg_anim
    elseif msg_anim<30 then
      y=0
    elseif msg_anim<38 then
      y=-msg_anim+30
    else
      y=-8
      msg=""
    end
    rectfill(0,y,128,y+7,8)
    print(msg,64-#msg*2,y+1,15)
    msg_anim+=1
  end
end
draw={}

function draw_topbar(str,c,right)
  c=c or 8
  rectfill(0,0,128,6,c)
  print(str,1,1,7)
  if (type(right)=="number") right=tostr(right)
  if right~= nil then
    print(right,(128-4*#right),1,7)
  end
end

function draw.menu()
  cls()
  map(0,0x20)
  draw_topbar(menu_title,8,menu_right)

  y=10
  for i=max(menu_pos-9,1),menu_pos-1 do
    print(menu_text[i],1,y,6)
    y+=6
  end
  y+=1
  --rectfill(0,y-1,128,y+5,9)
  print(menu_text[menu_pos],2-1,y-1,4)
  print(menu_text[menu_pos],2,y,9)
  y+=7
  local i = menu_pos+1
  while y<126 and i<=#menu_text do
    print(menu_text[i],1,y,6)
    y+=6
    i+=1
  end

  print("written by gpi",128-14*4,128-12,1)
  print("original by ed hobbs",128-20*4,128-6,1)
end

function dyinganim(p)
 p.stateanim+=1
 for i=1,15 do
   pal(i,rnd(3)/1+5)
 end
 spr(p.spr,p.x-4,p.y-3,1,1,p.flp)
 pal()
 if p.stateanim>=8 then
   p.state="death"
   p.x=0
   p.y=0
   setbox(p)
 end
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
 clip()
end

function mypal(x)
  pal(12,colors[x*2-1])
  pal(13,colors[x*2])
end

function draw_map()
  --background transport chamber
  for t in all(room.tmc) do
    local c=colors[t[1]*2-1]
    if (t.tele>0) c=rnd(colors) t.tele-=1
    rectfill(t.box[1],t.box[2],t.box[3],t.box[4],c)
  end

  mypal(room.col)
  map(16,1,0,8,16,15,1)--back
  map( 0,1,0,8,16,15,1)--mid
  pal(1,0)
  map(16,1,0,8,16,15,8)--inmid
  map(48,1,0,8,16,15,8)--inmid - waycorrect

  pal()
  --without palette
  map(0,1,0,8,16,15,4)
  --shifted
  map(0,1,0,8+3,16,15,2)

  --lightning
  for m in all(room.lma) do
    spr(0x35,m[1]*8,m[2]*8-3)
    if (m[3]==1) spr(0x6c+rnd(4)\1,m[1]*8,m[2]*8,1,2,rnd(2)>=0.5,false)
  end

  --forcefield
  frc_anim= frc_anim % #forcecolor +1
  pal(15, forcecolor[frc_anim])
  for f in all(room.frc) do
    if (f.activ<=1) spr(0x1d,f[1]*8,f[2]*8,1,2)
    spr(frc_spr[f.activ],f[3]*8,f[4]*8+3)
  end

  --doors
  for d in all(doors) do
    mypal(d.col)
    --clip(d.box[1],d.box[2],8,d.anim+2)
    spr(0x12,d.box[1],d.box[2])
    clip(d.box[1],d.box[2],8,8)
    spr(0x13,d.box[1],d.box[2]+d.anim)
    clip()
  end
 -- clip()

  --door bell
  for a in all(room.bel) do
    mypal(a.col)
    spr(0x50,a[2]*8,a[3]*8+3)
  end

  --locks
  for a in all(room.loc) do
    mypal(a[4])
    spr(0x59,a[2]*8,a[3]*8+3)
  end

  --tele dest
  for a in all(room.tmd) do
    mypal(a[1])
    spr(0x75,a[2]*8,a[3]*8)
  end

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
    --line(0,g.y+3,128,g.y+3,1)
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
    --rect(m[1]*8,m[2]*8+5,m[1]*8+0x0b,m[2]*8+0x0f)

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
      if (p.stateanim==8) p.state="nothere"
    elseif p.state=="outdoor" then
      p.stateanim+=1
      draw_player_door(p,9-p.stateanim,true)
      if (p.stateanim==8) p.state="alive"
    end

  end

  --foreground
  mypal(room.col)
  map(32,1,0,8,16,15,1)--front
  pal()

end
--[[
function _debugbox(t,c)
  rect(t[1],t[2],t[3],t[4],c)
end
--]]

function draw.room_play()
  cls()
  local r=""
  if (player[2]!=nil) r=timestr(player[2].time)
  draw_topbar(timestr(player[1].time),2,r)
  local x=64-4.5*#player.key
  for k in all(player.key) do
    spr(0x25+k,x,0)
    x+=9
  end

  draw_map()

  if (laby.name=="tutorial") printtutorial(roomnb)
  
  --debug
  --_debugbox(player[1].box,10)
  --_debugbox(player[1].obj.box,5)
  --[[
  for p in all(room.trp) do
    _debugbox(p.deathbox,8)
    _debugbox(p.switchbox,12)
  end
  --]]
  --[[
  for p in all(room.con) do
    _debugbox(p.movebox,2)
    _debugbox(p.switchbox,2)
  end
  --]]
  --[[
  for p in all(room.lma) do
    _debugbox(p.deathbox,8)
  end
  for p in all(room.swi) do
    _debugbox(p.switchbox,2)
  end
  --]]
  --[[
  for p in all(room.frc) do
    _debugbox(p.frcbox,8)
    _debugbox(p.switchbox,2)
  end
  --]]
  --[[
  for p in all(room.frk) do
    _debugbox(p.homebox,11)
  end
  --]]

  --if (dostop== true) stop()
end


---[[
function draw.laby_exit_anim()
 cls()
 map(0,0x20)
 local r=""
 if (player[2]!=nil) r=timestr(player[2].time)
 draw_topbar(timestr(player[1].time),2,r)

 for p in all(player) do
    if p.state=="walk1" or p.state=="walk2" then
      p.spr=0x60+flr(p.anim)
      p.flp=true
    else
      p.spr=0x56+flr(p.anim%2)
      p.flp=false
    end
    if (p.type==2) pal(15,4)
    spr(p.spr,p.x-4,p.y-3,1,1,p.flp)
    pal()
  end


end
--]]

function draw.show_map()
  cls()
  local r=""
  if (player[2]!=nil) r=timestr(player[2].time)
  draw_topbar(timestr(player[1].time),2,r)
 
  local w,h
  for r in all(laby.room) do
    r.xx,r.yy,r.ww,r.hh,r.cx,r.cy=r.x*8,r.y*8,(r.x+r.w)*8-1 , (r.y+r.h)*8-1,r.x*8+r.w*4,r.y*8+r.h*4
    if (r.visited) rect(r.xx,r.yy,r.ww,r.hh,7)
  end
 
  function _line(x1,y1,x2,y2,c)
    for x=-1,0 do
      for y=-1,0 do
        line(x1+x,y1+y,x2+x,y2+y,c)
      end
    end
  end
 
  for d in all(laby.door) do
    local r1,r2=laby.room[d[1]],laby.room[d[4]]
    if (r1.visited or r2.visited) _line(r1.cx,r1.cy,r2.cx,r2.cy,0)
  end  
  
  for r in all(laby.room) do
    if (r.visited) rectfill(r.xx+1,r.yy+1,r.ww-1,r.hh-1,colors[r.col*2-1])
    if (player[1].state!="quit" and laby.room[player[1].r]==r) print("웃",r.cx-4,r.cy-3,15)
    if (#player==2 and player[2].state!="quit" and laby.room[player[2].r]==r) print("웃",r.cx-3,r.cy-2,4)
  end
  print("you are here웃",71,123,15) 
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
  -- printh(%src.." "..tostr(src,1))
  for i = 2, %src do
    ii=tape_bits(bitsize)
    --printh("token:"..ii)
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

--block={0x8000,split("music,0xbe00,0x3100,0x1200,mainback,0xd000,0x1000,0x1000,endscreen,0xe000,0x1000,0x1000,"),0x8a8e,split("gfx,0xbe00,0x0000,0x1000,gfx_flags,0xce00,0x3000,0x0100,sfx,0xcf00,0x3200,0x1100,"),0x9474,split("levels,0xbe00,0x0000,0x4200,")}

function block_find(x)
  for i=1, #block,2 do
    local d=block[i+1]
    for a=1,#d,4 do
      if (d[a]==x) return block[i],d[a+1],d[a+2],d[a+3]
    end
  end
  assert(false,"blockfind")
end

function block_load(x,dont)
  local comp,src,dest,len=block_find(x)
  if (comp!=oldcomp) decompress(0x8000+0x3d00,comp) oldcomp=comp
  --print(x.."-"..tostr(comp,1).."\n:s:"..tostr(src,1).."\nd:"..tostr(dest,1).."\nl:"..tostr(len,1))
  if (not dont) memcpy(dest,src,len)
end


function create_block()
  local block_adr,s,v,e=0x8000+0x3d00  
  block={}
  repeat
    v=%block_adr block_adr+=2
    printh(tostr(v,1))
    if (v==0) break
    add(block,v)
    v=@block_adr block_adr+=1
    s=""
    for i=1,v do
      s..=chr(@block_adr) block_adr+=1
    end
    printh(s)
    add(block,split(s))
  until false
 
end

cls()
memcpy(0x8000,0x0000,0x4300)--,"creepbuild.p8")
create_block()
memset(0x0000,0,0x4300)
block_load "gfx"
block_load "gfx_flags"

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
9e888ae8e8bede8c8ee8e8fe000f18700800060483054100001a1003840644182511289426448315804a55aa244880080100540a08c1356a299465420c406466
050c204cb32218623a1c9003aa4099008c45239c22853146f973b13b8a29002ff25404893f262983235619a7729c0448100299025d700239049006630811c23b
89258c4190103cd82306405e2024aaa9a20840086ebce33b43003bb14794d52b0020c68d6107200876791558240a5108506b0807a2550ffee5d764907ce81f8e
d1b2000900d1da95c211eb57453ea205a92bc4a9bb94dbfac20cd781aad1d0100c2d71236202657abcaa5cac9d2108977cdcb64e8f67600b104e4dbb18a68caf
27c174b82f68009372c650a28eeca7f694d5b173664b331815b3468215c23d0e7b8fb79fe0fe77c5e7cf8f049e64d00c44ef6e1922c291c79c71073d75d1a916
f9009e79f7286ef9c04709779f6428dc50052156f1e7578be5007df960cfb173975058c56005955af0970540e777800d001c1e85a101a47283814d5a0097d68f
d111120dbf1cd488ea7dc60281b1215a19b850930910000c61140550d1452988841de4aff909583b00180050266019160698e03b100da1596902883a04e20aa0
00922633801704424d46e7173674ac8370e5900946a80986c20a04bb760000070d078300000e202037a6d000c05c10dc4040b5600a572a0c78f131ec06ab5651
59951a332bbc60ce8027080822883f82ee8ba8628daa33c0a26544464694278dc8e0a1ec16601451606100580aa2b300682a691002d80f822e00a8c928200469
0992a920117aa80ed7fd90ce06a7e6892144a9869c6a07ab72002d92e9679001228ebe6b236cc80c435e4660c569cf8ac5020cba9412f2fb977d01641cb28001
49d6cc290433c7812191ba153ca077c09f0b56481951f4ca9bf1101f6604175118ce1710cc7222762b51e9c566a06895cc8791c6476d6c00405850c500337fcd
fcf330c82437611dbfc28574d34b24b2494437530de4afa9385eaa6cf2d730b4c04324df431db1d333a58215d3f963d87dc34c83d2018432477db6d57de36753
ea974604bdda0486d94eb7be73fddb6dfb3100b028e0f289101c1e17e00dbd62712e1081812041360700395051b0e200e96f68d500850c2004b0ef930a78ab7e
88e4e10a06a89a9268c9314195599b0850c5c2024504c2100bb030cebbec6fe79e06f0003c70a7041820aeef000d19b2180914161060ff97fcf303023fb3897d
5090034c0c0efc007f96f00ded78a04cc9102f66f004faf2545149a141bbf5ae584bd73411e86f00f0090007d3c0f00c00dfdff604ff6000e1826e35f0d78213
561041a88a0420bb930788260501ddfed29fe813d8cf230b042d414052c1004a0344c083e9af608346d7b0c04010006c2ee5582618489bdc011bc1db431e4400
4000000300000e0a6720000a34408579ddaebcb40427ac100000eb20a10800af1f0331bfbc5612e0201d4e78c1a04309be00151a17ed58178b6c2e2a01c366cd
68e4000829b25fcb1145b5a1204e08837e806cc14120ea47d3e88974a19f400091e78a1b74660007c4b755230f5544e86b3c22f2ba4d021609e0cb10002404a9
300022d400040b126821036c02d5158034a022654085e1410e49140099a47075006881d7975c8bfe2c73f1c07bc152d6b9830b59f1008ac5490064a42cd2559c
62a2aa433859630071bc7f8f3b56b7afec0ad376021d94205dcf2f72a1507484e1f9465a59fd4910a433482712d47e403dc9ea4a9874a190453e451c16eadc43
5c9f3ca2770b3de9cb0e272af3b69d2678534827cc2c70a05e720cc0be688852064120445682b5e0244fd9684b85760482bf06b818232a1df2103a3989466339
801174a8469442d21ff8d0d94ff8f82f282d9413a21f018984a32c86b8281062971c2a25b4109d92a44020024d8936a108c1580415c034800b54e4f040886494
94e26cf1502788b413a05d9a9cd3830912e3d189d074e3519bca6ca241ef090a68254b690245d10e533cf3d62c5732524ea348c88c835b014648ae55655b5522
0bf70000a0a4cca908e6559486da00834670363650801be862175dea3fafb206a35f5f9467669dc8e19d4660b487e4eaa5ca5023ae70a45453787aa33142106f
a6de10f44c687486c76860047220140254dd0f8bc8cc40346a94a64a8db947a0c2c1b55c4d34080264bd444dddd67b83421d75a81001d6b6a4477d8b4965bf3d
777b1f36e820ad0b7c0fc682da0415a67b5b0ca6be74f49dcee219486c77fd646483de04ae56758acaaa0b1a4590808cb5f400836100b8926223bac32dd73eca
a9ac67fa55dadba50524594242c50f6b28da8dd2c1559d08839e414cf97f5883d51298b811485dc850f741bb228555e7ef983cb13e3f2e8176c11fc89dd17d9a
8035cde476975bd682e97d87460156c21388309b657cc196bb4698c64e8e32bf64475e785ac3e444a95f4a9bc110b10114e65851bc62e94f22398019002066e0
9300537acb66804d6010dcc6fb973c63688e3b00776b95ec24cac076d99d610c0de6bd917e4310d888b94800914008cdd58e3a9117470d40fbca969482d37054
b00006c137c21b00200d866c811038a1a61594d15f8a5a63a15d55f9d9a69935608c3dd976a7e8b4dc50055aeaea040b15c11b0830c40f441190b9c4d1b5400e
547fb40b0c61b5c27b506b8f276fada9f3b1d6969ad16f4b5d6bb0bd55c66508c2608524101b7c263066fad0679b7eecbdcd024a6bd6f260811c6dcbd0bb48c2
01010c0c492cc4ed62e70009eb53bc73fd600006f6e0efde8769d180c90e52510afa2cf32587f55178040778493f18eb31f1582ed752e4206c13239480af8b00
201234ee9f8c0480c140593ae927ac95779f6ed2fbc1379906e10c1130320c81c9f206c80321830c000c3ba74f5558e3a71d2a626243d08c0310044bf925a4f4
74d5578a5ae3a75d757addae7a76750840a30d0c9e51304a67ca5be3b79d7672d5a4080705105043f0bfee663ad1779b4de10000a18bd1309bfbeb7ffd3ebda7
30d9860077d0dfeab71e767eebeee080f4871ef077cbee35070a10d091fdcf3e787b0aeed047ff103eb27fc7ee680e3ad8b0e20600a37d8b3e20b0cd0dd8b3e2
8b04208cc83ef8b0e2042c334aeb3220cd30fbbffe577ed77ffb70fdb70e250b2110aa892ea0828a0d9872ea8824a029842a88826a07e789701bf1bfdef5f9d7
6f9b7fefbfddb0009861630f0c62798d2e20b005009832a28a002068802698922008b842f97090ff84841afa60600340806161080a800841a8033250805b0100
890808b0d08a8c0808f0118e8018183135220a112083c30b07202083c20501201800128f802828325282842828729286882820b28a01773703e1af2620383895
338289223853ed168838a193b38d1c383803f38e85114814311284481154748118484830b48a8e4748e405808f1748254583841158d485808e4848a5c58b8848
48e5068582483826468f8c3838668689863838a6c683803817e6078e8917172747848f070767878a8507f6a7c78f29f600af98b05465194226ce5c2110010013
b0c9911c8003c208f87a59d89ab5da2328e5f0b12b40234c3310e9e15264199766e588b5c4f8bd20015184703181098a0a0054ed08f6a079250c0b00c3001db1
17c2c00861e57891a08ca3222d88b32a44fa18fa9301858fe16700018009edc490cd00307e370e1c80015aa78019287e0b0683a0f8c209fe1088d20d90e46411
0c57035e00e098c228fad886b30a4861e254b1670e4ec6b6894015b05eb629863b51a66ca554c4dc00b6b92b121c5825286c500110088d2e6688838c69e18632
92445287007710e42340c7ce30c1798b631048a2ee06c9d58834127e848fe071d5e839cc1419bbc207f5bcc1b919245377860871145407af8061a9b2da68b44d
c480b170fcd90972db85958751101aec3a0a4c96517201206e8a64120a19e23113bcfa9290208238b0b103580687d4b2453451e42d287e0a10028f02107a2b78
0a88f8810f2222541260a74bd8a00304a0d917b0939130c1c83a623a23d06309e8a10c08e30d24943046b0a20431200929081f485442870163c13d04b005dce2
0011c1ac20607843c4230a1421980320158104518b3fd3503ad0190438218a0a626219a0fe4310b6840790493d5c000c0492461381ac05ac600cc8010b025043
0100294838a04120324a0e480443cc630e1ee29f1ae4b78044518d3dc30b3ec8550554424b22a01526d80b4e5082003411303d0459464451c116c35e0510250c
7032032722443d98e604c800051b63bc3a40a908a8a28906836824a0d946b0c0061381563450d648d02345350051092073c37cd30c1910c017983fd944620235
d0611490a441e0704cb600a41ce441802c600b31102e1c04964980100f26c0d733b8fbc384c24a0c01a211c4a5875473813da26c2d20258a50408a13b3c630d0
274468e18c3c821d32043e0c2083090302223c5c71cb2c930e2a212714d854c30862c000501c00880d08dc00872091361b8460c54003003a60e314d424053461
492140590b5cd18cd0308d13319007d054c28c21cc2803b10a04c4848013450d52c03ac0d480b4c34828a25c09c483c960528505134b22b00e4800d14501015b
0e70be4a3c100700e20b09bc5b8d14038629802b3e5c890a1442892890de12fc66cbc453c431d20d2570a5495cb1ce1e427105a0600208a04135e04024007e4f
a0a1ca1cb025308c408600804811a0a226241b8ab0724834a28e37b0a7c5185e8906e2292da48847a0630a31a198056c4b056801413252ba3b4cacc980b28005
005b2b1011408012081883382dc23785b873c3288065c2329812680160f298850a86e38a0790e2248500213c080e70910b0a403325810080b68413d803018068
600f43245042801114a39881103010c1074001c70814214d09222080221010f0428c14807206131880410e64c107c90aa1456c017000fd023250b784084890a4
024000868520e031cb107c00958300b047c115c812888560d088c62818052f0d4ca01f9a0290c4ce1f085306004867080e1a88a18a1028d0a3031a2091cc0630
61a60904017c4b0838814112686113882001f68909b873e68244b16f49242002a00f109199050ee184000d00a0a08444d009c2042855a01518615a073cf0d59c
12a8a08a890c807d09267862090c2c717a033820264b18a8622e0920a0a8051c28c5450a6052b61c209020c013000248082c900a4232888760113420028c2a31
f6831a18b24c8354a1cd0620308621014462768530219707074b90260644812d410668c223154471690b2800f2840020e08982189000840ce002200630b18205
3e81c3490d504280027041c0491a488203021071d9023051f3c508a000ee838401574412380424050050010400718604158040008500c0780e26809024024870
00831280048b12504207802c50779010d03768060010ec08281131850e28810b0420119c440c70712c1a60a2680e1470004a11e830c384488173c2126810a41a
0400428638b0b54b07a070ca8560e09202222060a118741139051cd0604d0c6012a4093c41628b082044c8066842da01265060cb0528436700185127c8226830
4f0750818508220025c201c8f0658660c1c0836560310b1504d0210f08206147044803cf891830d1ca2230e4060108b0a0841e90934c1f1095700d20d1700c04
6026410e78c2ae033241a0401cb0a3c0883cf07b051c686446194022b6863030648c1ca091450548502c4c1a70c4c80d24620e84106021410cb0b1658608210a
080c18d4a3197ca2b1840551054e0580e3e68e78a05a8d0650d406040c20ca851c81e5441998904c0510106706160884a4080452318f08e0100f1f6871a43220
010d4020e0a0051448110a0c301177031610a1ad0a541181833648b5ef082022e4022011334a077813078d4061a407b84051e40048a02e032810048205c04325
801c21a709046892c6033411008a0650954d0230a04c04100158801620112c0808811a0c064102480818920382748016c602b0f03c1200d6e88e2210d40a1880
03a48508a046013080a2c61a5053e10432e1078a0748428682700189023a60c4480564139a0412e0b2c4125802448108704c023a9027ee0630c157061800f9ca
192000008144a097060c1016640620b02c840621624b19d826328e5081bb8134e841a1194852798402e101ca1cf0f1838864d1c2c820d0a5880b008094093e90
d24203a861878568817005004002400010322d88005074ca1b20c2290a4c9023420036e4aa106cf2848e1e6072cb0bd840e889000089053ae800e01e34817789
3a31f2891270e2e60010e0450c16c4e460061c736a882240f8480d8830a989388111381060b165aa58d0410c3000524a18583284893c501f8c2eb857ec0200b2
2d832cc0c14819e01000820c9086cb11081005015402e90732d1850b1c5d0222811010be492ee821050028e90000040066410370a0648c2001fa8c3cc8514903
406138840ac082880e5850c1095040a60f1228e227130482c2003081b146145810c9852c4008c6ce5804ef1438c33e080040304418c0b1610b50d020ca149880
250040d1a3130cc061430208d6e10d4c8124061e0041a80104428601382111801b8853aa836000440d3c1025a30a3083160b00c050c406f000290d105009013e
80110d1408614b89241012490de8f22a0a20d1f2c716a8830f0428513e0e2c715643045010c20f044048c02c80714712180252040690804718c002c805406166
02322853c91b309007003cc033021a883061ca4060b213020260a073db0a0150000c1a000c909029720105106056fc0701b010d2a705008000a01e04003020d8
e40008b020ec900355103073ac0e09f490885c0405800071ab0700308041220a0b504071bd3f0e10805c13090c8050b52e0b0060803ef90c0680300c900f0380
d092f00a01109097650d094070488a0102f080b093040e90002142070500c082d30b084000ee37070600e0691d040330a0189a01040030d676030c50e004c50e
08f300d2fc040db000369f0d0390e4b060000040d09b530c01c061056c030620d0ea2c0004b08091030604a00046e00700b010751108010000c7df0607a0b06f
3f0b0ad0705f8e000ba0d03fd50c0c60e0e679090c60603b150643c0c0528f0a00a0909170080900f00207040230000c2a0a0580704e2b04000020a9e7050d30
002e180f0b000090130901b020e8d604023040806d0800e0001061000400800ef06e0a2020ab4b0609c060ac9d050e60604b080a06d080043b8f0a0070313f03
0bd630e27502091001f2e70e072010dd000e06606014f00a0ce020273f0b00605087360e04a0403626060b9080c7ac0b0ce0f07d6f0a004050a8710001e610ce
2002011060587c080a304074200c0830902901007f644095270000806069b60901e0109c1906000010e901000d8030cdc80f0810908974030c7080f517000400
20ec210a06a020a512083c60c07c110d0850d3ca300f01d0f000980d0a6040f9560304c0a0a5ba0d05c030290c0c0400104bca0201d0102ba20c09d0901f1900
217940845901086010050c0a00b05035250c00d0400b130c0da020642c0008a0980001000b20b01d4909059060b39b080110200fc50a0810109529040b0050b2
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
5ff017050ad905e080000884d950900f01d269d0200d035492b0600b0499e8c0950204e5ca60600404aa8960a0040e691f0090090a2b7410500008ec046050060b4e48a0960e05ee4660900e0589858020090c55280080000b497760c006047d57502000008aaae0a009048c98c0400c08a4c74060040f6a17c0a00b005ab567
f02a0507c880e0092e8aef1070060dbfb7300000007acb00030808bec5e0b00607a2b250a009009d62c0300106470040b00d078907200104067e8510d00e0211bab0a0030ba45910100109537600060208481d9010090f4c6340c00e02905aa0600e01a26b9020090a518970e00308055180d008045d6950500404049b60800b
023b8d3050050e8e0a10c0000d17f8500105086bc8a0800800888600f00d00222e20d00104035bb070080e0bc420b00b07f11c30800e03121028d0040fc5922000020c21d4d0200100ee4d34c00c0f5c5e30f0090a280e80f00b0c182e80700f003d48d0c0040742006000000baaf620000600011e90600707a0aae0e00e0567
95b6800c073151d09008005355b0d0000493c200100e09d2d630200b0bda0060a00b0f083d308001040a7020f0007e861420300809a10b80200c0161eef0001b01198fa025010c46bd40200307e1f180e00c084f6030800900f00580100e0726482020090833beca400a0219483000024c4f72d0003709852be0f0090a90ee90
a00206939bb030020d3705c00002012c56d0800003a54f441445455357549445455b5fd4144646636754740746696db4f44647717534744747797db4f44748818534744848898db4f44849919534744949999db4f4494aa1a534744a4aa9adb4f44a4bb1b534744b4bb9bdb4f44b4cc1c534704c4c8ecb94d44c4dcfd314544d
4dd7db94d44d4edfe314544e4ee7eb9434064eedf1f4344f4ff5f974b44f4ffd01f5355050050975b550500d11f5355151151975b551511d21f5355252252975b552522d31f5355353353975b553533d41f535540145cc00ca08b88e854b6bb8b887898babb8b88b8dcbebb8b98f910b2bb9b993954b6bb9b997998babb9b99b
9dcbebb9ba9fa10b2bbabaa3a54b6bbabaa7a98babbabaabadcbebbabbafb10b2bbbbbb3b54b6bbbbbb7b98babbbbbbbbdcbebbbbcbfc10b2bbcbcc3c54b6bbcbcc7c98babbcbccbcdcbebbcbdcfd10b2bbdbdd3d54b6bbdbdd7d98babbdbddbddcbebbdbedfe10b2bbebee3e54b6bbebee7e98babbebeebedcbebbebfeff10b
2bbfbff3f54b6bbfbff7f98babbfbffbfdcbebbfc0ff010c2cc0c003054c6cc0c007098cacc0bb0b4190200404430e4c0cc1c1120f1c5cc1c113146cacc1c11918ccecc1c2171b0cfcc1c2221d1c5cc2c223246cacc2c22928ccecc2c3272b0cfcc2c3322d1c5cc3c133c26b8b8d01c3c40bdcc3c33c41ec0cc4bc433f2c5cc4
c442447c1cbcc44648bcdcc4bc4a4c0c1cc5c54e503cfcbbc552547c9cc5bb5658ecdcc5c55a5cfcdcbbc65e603c5cc6bb6264cc9cc6c66668bcbcbbc66a6cfc1cc7bb6e70ac5cc7c772747c9cbbc77678bcdcc7bb7a7c8c1cc8c87e803c7cbb003f000000000000000000000000000000000000000000000000000000000000
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
30302c7230003300623004731003300032c436261471f1362c506275172c0033851624103300623004733003300032c00338103310033006233136381623004723526300032c0033851332003300623004731403
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

