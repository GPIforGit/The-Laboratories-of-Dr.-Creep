pico-8 cartridge // http://www.pico-8.com
version 33
__lua__
--creep edit
-- by gpi 2021

poke(0x5f36,16) -- enable high memory

-- load sprites and flags
reload(0,0,0x2000,"creep_sprites_sound.p8")
reload(0x3000,0x3000,0x100,"creep_sprites_sound.p8")

--cstore()
mainfile="creep_levels.p8"
buildfile="creepbuild.p8"

function tape_load()
  reload(0x8000,0x0000,0x4300,mainfile)
  tape_rewind()
end
function tape_save()
  cstore(0x0000,0x8000,0x4300,mainfile)
  --load(buildfile0)
  tape_rewind()
end
function tape_clear()
  memset(0x8000,0x00,0x2000)
  tape_rewind()
end
function tape_rewind()
  tape_pos=0x8000
  tape_bit=0x8000
end
function tape_rewind_save()
  tape_pos,tape_end,tape_bit=0x5e00,0x5f00,0x8000
end


function tape_backbit(x)
  for i=1,x do
    tape_bit<<=1
    if (tape_bit>128) tape_bit=1 tape_pos-=1
  end
end

tape_doclip=false

function tape_bits(x,byte)
  local write,ret=0.5<<x,0
  if tape_doclip then
    if byte then
	     clipstr..=chr(93+byte)
	   else
	     ret=ord(clipstr)-93
	     clipstr=sub(clipstr,2)
   	end
  else
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

function tape_save_laby(oldname)
  local complete,save,saved={},laby,false
  --tape_clear()
  tape_rewind{}
  while tape_read_laby() do
    if laby.name != save.name and
      laby.name!=oldname then
      
      add(complete,laby)
    elseif save.name!="" then    
      add(complete,save)
      saveed=true
    end
  end
  if (not saveed and save.name!="") add(complete,save)
  tape_rewind()
  for l in all(complete) do
    laby=l
    tape_write_laby()
  end
  tape_setstr("")--endmark
  laby=save
  tape_save()
end

function tape_list_laby()
  local list,save = {},laby
  tape_rewind()
  while tape_read_laby() do
    add(list,laby.name.." ("..#laby.room.." rooms)")
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

function box(x,y,w,h,c)
  rect(x,y,x+w-1,y+h-1,c)
end
function boxfill(x,y,w,h,c)
  rectfill(x,y,x+w-1,y+h-1,c)
end

--[[

state
roomnb = currentroom
room = room-direct!
deco
doors
labylist

menu_text
menu_pos
menu_retstate
menu_title

laby
  .name
  .room
    .col (=0 endmark)
    .way l,x,y (l=0 endmark)
    .lad l,x,y (l=0 endmark)
    .pol l,x,y (l=0 endmark)
    .bel doornb in room,x,y id1
    .loc doornb in room,x,y,c id2
    .con d,x1,y1,x2,y2 [anim]
    .lma x,y,on
    .swi x,y,on,mask
    .key c,x,y
    .frc x1,y1,x2,y2
    .trp x1,y1,x2,y2,on
    .tmc c,x,y
    .tmd c,x,y - color is not stored!
    .mum x1,y1,x2,y2
    .frk x,y,d
    .gun l,x1,y1,p,x2,x3,d
  .door
     r,x,y,r,x,y [off][c][linkroom][#global][#doors]
--]]
-->8


function new_room()
  local a= {col=1,x=1,y=1,w=1,h=1}
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
    player ={1,1,1,2,1}     
  }
end


function _readtable(lin)
  local c={}
  for i=1,#lin do
    add(c,tape_bits(sub(lin,i,i)))
  end
  add(room[what],c)
  return c
end
function _read(what,lin)
  for a=1,tape_bits(4) do 
    add(room[what],_readtable(lin))
  end
end

function _writetable(t,lin)
  for i=1,#lin do
    tape_bits(sub(lin,i,i),t[i])
  end
end

function _write(what,lin,id)
  if id ~= nil then
    if (#room[what]==0) return false
    tape_bits(4,id)
  end
  tape_bits(4,#room[what])
  if (#room[what]>15) printh("to many! "..what)
  for x in all(room[what]) do
    _writetable(x,lin)
  end
  return true
end

labymatrix=split("1,bel,444,2,loc,4443,3,key,344,4,con,24444,5,lma,441,5,swi,4418,6,frc,4444,7,trp,44441,8,tmc,344,8,tmd,044,9,mum,4444,10,frk,441,11,gun,4444441",",",false)

function chr96(x)
  return chr(91+x)
end

function tape_write_laby()
  tape_setstr(laby.name)
  tape_bits(5,#laby.room)
  for r in all(laby.room) do
    room=r
    tape_bits(4,r.col)
    tape_bits(4,r.x)
    tape_bits(4,r.y)
    tape_bits(3,r.w)
    tape_bits(3,r.h)
   _write("way","444")
   _write("lad","444")
   _write("pol","444")
  --variabel
   local ok=false
   for i=1,#labymatrix,3 do  
  if labymatrix[i]==labymatrix[i-3] then
    if (ok) _write(labymatrix[i+1],labymatrix[i+2])
  else
    ok=_write(labymatrix[i+1],labymatrix[i+2],labymatrix[i])
     end
   end
  
    --endvariable
    tape_bits(4,0x0) --endid
  end
     
  --doors  
  for d in all(laby.door) do
    tape_bits(5,d[1])--room1
    tape_bits(4,d[2])--x1
    tape_bits(4,d[3])--y1
    tape_bits(5,d[4])--room2
    if d[4]!=d[1] then
      tape_bits(4,d[5])--x2
      tape_bits(4,d[6])--y2
    end
  end
  tape_bits(5,0)--endmark
  
  -- player
  _writetable(laby.player,"54444")
end
function _correct(what)
  for w in all(room[what]) do
    if (w[1]==0) w[1]=16
  end
end
function tape_read_laby()
  local n,c,r= tape_getstr()
  if n=="" then
    laby=nil
    return false
  end
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
    _read("lad","444")
    _correct("lad")
    _read("pol","444")
    _correct("pol")
 
 
 repeat
   c=tape_bits"4"
   for i=1,#labymatrix,3 do
     if (c==tonum(labymatrix[i])) _read(labymatrix[i+1],labymatrix[i+2])
   end
 until c==0
 _correct("gun")
    
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
      if mget(p[2],i)==2 then
        mset(32+p[2],i,0x7b) 
        mset(p[2],i,0x15)
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
     if d[1]==d[4] then
       d[7]=2
       d[8]=5
       d[9]=roomnb
       msets(d[2]+16,d[3],2,2,0x08)
     elseif d[1]==roomnb then
       d[7]=2
       d[8]=laby.room[d[4]].col
       d[9]=d[4]
       msets(d[2]+16,d[3],2,2,0x06)
     else
       d[7]=5
       d[8]=laby.room[d[1]].col
       d[9]=d[1]
       msets(d[5]+16,d[6],2,2,0x06)
     end  
     d[10]=i
     d[11]=#doors
   end
 end 
 --bel
 for b in all(room.bel) do
   if (doors[b[1]]==nil) del(room.bel,b)   
 end
 --loc
 --for l in all(room.loc) do
 --  mset(l[2],l[3],0x59+l[4]-1)
 --end
 --key
 for k in all(room.key) do
   mset(k[2],k[3],0x25+k[1])
 end
 --con
 for c in all(room.con) do
   mset(c[4],c[5],0x4c+c[1])
 end
 --lma
 foreach(room.lma,draw_lma)
 d=1
 for m in all(room.lma) do 
   m[4]=d d+=1
 end
 --swi
 for s in all(room.swi) do
   mset(s[1],s[2],0x49+s[3])
 end
 --frc
 for f in all(room.frc) do
   mset(f[1],f[2],0x0d)
   --mset(f[3],f[4],0x2d)
 end
 --trp
 for t in all(room.trp) do
   mset(t[1],t[2],0x30+t[5]*3)
   mset(t[3],t[4],0x5a+t[5])
 end
 --tmc
 for t in all(room.tmc) do
   msets(t[2]+32,t[3],2,2,0x10)
 end
 
 --tmd
 d=1
 for a in all(room.tmd) do
   a[1]=d d+=1
 --  mset(d[2],d[3],0x75+d[1]-1)
 end
 
 --mum
 for m in all(room.mum) do
   msets(m[1]+16,m[2],2,2,0x0e)
   mset(m[3],m[4],0x23)
 end
 --frk
 for f in all(room.frk) do
   msets(f[1]+16,f[2],1,2,0x0a)
   msets(f[1]+32,f[2],1,2,0x0b + f[3])  
 end
 --gun
 for g in all(room.gun) do
   for y=g[3],g[3]+g[1]-1 do
     mset(g[2],y,0x4b+g[7])
   end
   --mset(g[5],g[6],0x40)
 end
 
 laby_draw_anim_map()
end

function laby_draw_anim_map()
 
  --con
  for c in all(room.con) do
    if c[1]==2 then
      c[6]=(anim_cycle%4)
    elseif c[1]==3 then
      c[6]=3-(anim_cycle%4)
    else
      c[6]=0
    end
    msets(c[2],c[3],2,1,0x36+c[6]*2)
  end
  
  --animate powerline
  local adr,save=3040, peek4(3040)
  for i=0,6 do
    poke4(adr,peek4(adr-64))
    adr-=64
  end
  poke4(adr,save)  
end


function obj_under(mx,my)
  for d in all(doors) do
    if (d[d[7]]<=mx and d[d[7]+1]<=my and mx<=d[d[7]]+1 and my<=d[d[7]+1]+1) return "door",d
  end
  
  for p in all(room.lad) do
    if (p[3]<=my and my<p[3]+p[1] and mx==p[2]) return "lad",p
  end
  
  for p in all(room.pol) do
    if (p[3]<=my and my<p[3]+p[1] and mx==p[2]) return "pol",p
  end
  
  for p in all(room.bel) do
    if (p[2]==mx and p[3]<=my and my<=p[3]+1) return "bel",p
  end
  
  for p in all(room.loc) do
    if (p[2]==mx and p[3]<=my and my<=p[3]+1) return "loc",p
  end
  
  for p in all(room.key) do
    if (p[2]==mx and p[3]<=my and my<=p[3]+1) return "key",p
  end
  
  for p in all(room.con) do
    if (p[4]==mx and p[5]<=my and my<=p[5]+1) return "con",p,2
    if (p[2]<=mx and mx<=p[2]+1 and my==p[3]) return "con",p,1
  end

  for p in all(room.lma) do
    if (p[1]==mx and p[2]-1<=my and my<=p[2]+1) return "lma",p
  end
  
  for p in all(room.swi) do
    if (p[1]==mx and p[2]<=my and my<=p[2]+1) return "swi",p
  end  

  for p in all(room.frc) do
    if (p[1]==mx and p[2]<=my and my<=p[2]+1) return "frc",p,1
    if (p[3]==mx and p[4]<=my and my<=p[4]+1) return "frc",p,2
  end
  
  for p in all(room.trp) do
    if (p[1]==mx and p[2]==my) return "trp",p,1
    if (p[3]==mx and p[4]<=my and my<=p[4]+1) return "trp",p,2
  end  

  for p in all(room.tmc) do
    if (p[2]<=mx and mx<=p[2]+1 and p[3]<=my and my<=p[3]+1) return "tmc",p
  end
  
  for p in all(room.tmd) do
    if (p[2]==mx and p[3]<=my and my<=p[3]+1) return "tmd",p
  end

  for p in all(room.mum) do
    if (p[1]<=mx and mx<=p[1]+1 and p[2]<=my and my<=p[2]+1) return "mum",p,1
    if (p[3]==mx and p[4]<=my and my<=p[4]+1) return "mum",p,2
  end
  
  for p in all(room.frk) do
    if (p[1]==mx and p[2]<=my and my<=p[2]+1) return "frk",p
  end
  
  for p in all(room.gun) do
    if (p[2]-p[7]<=mx and mx<=p[2]-p[7]+1 and p[3]+p[4]==my) return "gun",p,3
    if (p[2]==mx and p[3]<=my and my<p[3]+p[1]) return "gun",p,1
    if (p[5]==mx and p[6]<=my and my<=p[6]+1) return "gun",p,4  
  end 
  
  
  for p in all(room.way) do
    if (p[2]<=mx and mx<p[2]+p[1] and my==p[3]) return "way",p
  end
  

  return "",nil
end

function obj_box(t,o,a)
  if t=="way" and objtype!="con" 
    and objtype!="pol" 
    and objtype!="lad" 
    and objtype!="trp" then
    return o[2]*8,o[3]*8,(o[2]+o[1])*8-1,o[3]*8+7
  elseif t=="pol" or t=="lad" then
    return o[2]*8,o[3]*8,o[2]*8+7,(o[3]+o[1])*8-1
  elseif t=="door" then
    return o[o[7]]*8,o[o[7]+1]*8,o[o[7]]*8+15,o[o[7]+1]*8+15
  elseif t=="con" then
    if a==2 then
      return o[4]*8,o[5]*8,o[4]*8+7,o[5]*8+15
    else 
      return o[2]*8,o[3]*8,o[2]*8+15,o[3]*8+7
    end
  elseif t=="tmd" or t=="key" or t=="bel" or t=="loc" then
    return o[2]*8,o[3]*8,o[2]*8+7,o[3]*8+15
  elseif t=="tmc" then
    return o[2]*8,o[3]*8,o[2]*8+15,o[3]*8+15
  elseif t=="lma" then
    return o[1]*8,o[2]*8-8,o[1]*8+7,o[2]*8+15
  elseif t=="frk" or t=="swi" then
    return o[1]*8,o[2]*8,o[1]*8+7,o[2]*8+15
  elseif t=="frc" then
    if a==1 then
      return o[1]*8,o[2]*8,o[1]*8+7,o[2]*8+15
    else 
      return o[3]*8,o[4]*8,o[3]*8+7,o[4]*8+15
    end
  elseif t=="trp" then
    if a==1 then
      return o[1]*8,o[2]*8,o[1]*8+7,o[2]*8+7
    else 
      return o[3]*8,o[4]*8,o[3]*8+7,o[4]*8+15
    end
  elseif t=="mum" then
    if a==1 then
      return o[1]*8,o[2]*8,o[1]*8+15,o[2]*8+15
    else 
      return o[3]*8,o[4]*8,o[3]*8+7,o[4]*8+15
    end
  elseif t=="gun" then
    if a==1 then 
      return o[2]*8,o[3]*8,o[2]*8+7,(o[3]+o[1])*8-1
    elseif a== 3 then
      return (o[2]-o[7])*8,(o[3]+o[4])*8,(o[2]-o[7])*8+15,(o[3]+o[4])*8+7
    else
      return o[5]*8,o[6]*8,o[5]*8+7,o[6]*8+15
    end
    
  elseif objtype=="con" then
    return mx*8,my*8,mx*8+15,my*8+7
  elseif objtype=="mum" or objtype=="tmc" or objtype=="door" then
    return mx*8,my*8,mx*8+15,my*8+15
  elseif objtype=="frk" or objtype=="tmd" or objtype=="frc" or objtype=="swi" or objtype=="key" or objtype=="loc" or objtype=="bel" then
    return mx*8,my*8,mx*8+7,my*8+15
  elseif objtype=="lma" then
    return mx*8,my*8-8,mx*8+7,my*8+15
  end
  return mx*8,my*8,mx*8+7,my*8+7
end



function isempty_room()
  return  #room.way==0 and #doors == 0
end


function calc_save()
  local s= 5+4+4+2+5+4+4+2 --playerpos & lives
         + #laby.door
  for r in all(laby.room) do
    s+=#r.key
      +#r.con*2
      +#r.lma
      +#r.swi
      
  end
  return s
end

forcecolor={7,15,10,15}
colors=split("11,3,6,5,8,2,14,2,10,9,12,13,4,2,0,1,13,1,15,14,9,4,3,5,5,0")
frc_spr=split("0x7a,0x79,0x78,0x77,0x76,0x5f,0x5e,0x5d,0x5c")   
   
-->8
--init & update
function _init()
  tape_load()
  --tape_clear()
  
  state="main"
  labylist=tape_list_laby()
  poke(0x5f2d, 0x3) --mouse and keyboard
  
  anim_cycle=0
  
end


function _update()
  anim_cycle +=1
  mxx,myy = stat(32),stat(33)
  mx,my = mxx\8, myy\8
  mb = stat(34)
  if mb!=oldmb then
    mclick = mb
    oldmb=mb
  else
    mclick = 0
  end
  
  key = stat(31)
  
  if update[state]!=nil then
    update[state]()
  else
    cls()
    ?"unknown state:"..state
    stop()
  end
end

function switch_room(x,news)
  if roomnb>#laby.room and
      not isempty_room() then
    laby.room[roomnb] = room
  end  
  roomnb = x
  state=news or "level_init"  
end

update={}

function update.main()
  menu_text={}
  for i in all(labylist) do
    add(menu_text,i)
  end
  add(menu_text,"<new labyrinth>")
  menu_pos=1
  menu_retstate="main_selected"
  menu_title = "select labyrinth"
  menu_right= ""
  state="menu"
  
  if stat(100) then
    tape_rewind_save()

    local str,nb=tape_getstr(),tape_bits(5)
    tape_rewind_save()
    tape_bits(16,0)
    if str!="" then
      update.main_selected(str,nb)
    end
  end
  
end

function update.main_selected(str,nb)
  tape_find_laby(str or menu_text[menu_pos])
  
  roomnb = nb or 1
  state="level_init"
    
  menuitem(1,"build", function()
    tape_rewind_save()
    tape_setstr(laby.name)
    tape_bits(5,roomnb)
  
    switch_room(roomnb)
    tape_save_laby()       
    load(buildfile,"editor")
  end)
end

function update.level_init()
  roomnb = mid(1,roomnb,min(31,#laby.room+1))
  room=laby.room[roomnb]
  if (room==nil) room=new_room()
  laby_draw_map()
  state = "level_edit" 
  obj = nil
  objtype = objtype or "way"
end

function _add_obj(k,v,a)
  if (#room[k]>=15) return
  add(room[k],v)
  if a~=nil then
    obj=room[k][#room[k]]
    obj_action=a
  else
    laby_draw_map()
  end
end

function update.level_edit()

  -- animations
  laby_draw_anim_map()
  
  function _change_color(t,o)
    if t == "way" then
      if (key>="1" and key<="9") room.col=key-0
      if (key=="0") room.col=10
      if (key=="/") room.col=11
      if (key=="*") room.col=12
      if (key=="," or key==".") room.col=13
    elseif key>="1" then
      if t=="loc" and key<="7" then
        o[4]=key-0
      elseif (t=="tmc" or t=="key") and key<="7" then
        o[1]=key-0
      elseif t == "con" and key<="3" then
        o[1]=key-0 
      elseif (t == "frk" or t == "swi" or t == "lma") and key<="2" then
        o[3]=key-1
      elseif t == "trp" and key<="2" then
        o[5]=key-1 --zerobased!  
      elseif t == "gun" and key<="2" then
        o[7]=key-1 --zerobased!  
      end  
    end
    -- redraw map
    laby_draw_map()
  end

  if my<1 then
    if mxx>=0 and mxx<=8*4 then
      if (mclick & 1 != 0) switch_room(roomnb+1)
      if (mclick & 2 != 0) switch_room(roomnb-1)
    end
    return false
  end
  

  if mx<0 or mx>15 or
     my<1 or my>15 then
    return false
  end
  
  -- edit object
  if obj != nil then
 
    if objtype == "way" then
      if obj_action==2 then
        obj[1]=max(1,mx-obj[2]+1)
      else
        obj[2]=mx
        obj[3]=my
      end
    elseif objtype == "lad" 
        or objtype == "pol" then        
      if obj_action==2 then
        obj[1]=max(1,my-obj[3]+1)
      else
        obj[2]=mx
        obj[3]=my
      end
    elseif objtype == "door" then
      obj[obj[7]]=mx
      obj[obj[7]+1]=my
    elseif objtype == "bel" or objtype=="loc" then
      if obj_action==2 then
        local t,o = obj_under(mx,my)
        if t=="door" then
          obj[1]=o[11]
        end
      else
        obj[2]=mx
        obj[3]=my
      end
    elseif objtype == "tmc" or objtype == "tmd" or objtype == "key" then
      obj[2]=mx
      obj[3]=my
    elseif objtype == "con" then
      if obj_action==1 then
        obj[2]=mx
        obj[3]=my
      else
        obj[4]=mx
        obj[5]=my
      end
    elseif (objtype == "frk" or objtype == "lma") then
      obj[1]=mx
      obj[2]=my
    elseif objtype == "swi" then
      if obj_action==1 then
        obj[1]=mx
        obj[2]=my
      elseif obj_action==2 then
        local t,o = obj_under(mx,my)
        if mclick & 1 !=0  then
          if t=="lma" then
            obj[4] ^^= 0.5<<o[4]
            mclick=0
          end          
        end
      end  
    elseif (objtype=="mum" or objtype == "trp" or objtype =="frc") then
      if obj_action==1 then
        obj[1]=mx
        obj[2]=my
      else
        obj[3]=mx
        obj[4]=my
      end
    elseif objtype=="gun" then
      if obj_action==1 then
        obj[2]=mx
        obj[3]=my
      elseif obj_action==2 then
        obj[1]=max(1,my-obj[3]+1)
      elseif obj_action==3 then
        obj[4]=mid(0,my-obj[3],obj[1]-1)
      elseif obj_action==4 then
        obj[5]=mx
        obj[6]=my
      end
    end
  
    -- set
    if mclick & 3 ~= 0 then
      if objtype == "gun" and
         mclick & 1 ~=0 and 
         obj_action<4 and 
         obj[5]==-1 then
        obj_action+=1
      else       
        obj=nil
      end
    
    -- change color/settings
    elseif (key>="0" and key<="9") or 
            key=="/" or key=="*" or key=="," or key=="." then
      _change_color(objtype,obj)
      
    -- delete    
    elseif key=="\08" then
      if objtype=="door" then
        local i=1
        while i<=#room.bel do
          local b=room.bel[i]
          if b[1]==obj[11] then
            deli(room.bel,i)
          else
            if b[1]>obj[11] then
              b[1]-=1
            end
            i+=1
          end
        end
        i=1
        while i<=#room.loc do
          local l=room.loc[i]
          if l[1]==obj[11] then
            deli(room.loc,i)
          else
            if l[1]>obj[11] then
              l[1]-=1
            end
            i+=1
          end
        end        
        del(laby.door,obj)
      else
        del(room[objtype],obj)
      end
      obj=nil
    end
    
    -- redraw map
    laby_draw_map()
    
  elseif (key>="0" and key<="9") or 
    key=="/" or key=="*" or key=="," or key=="." then
    -- change background-color
    _change_color(obj_under(mx,my))
  
  -- draw/select a object 
  elseif mclick & 2 ~=0 then
    local t,o = obj_under(mx,my)
    if t=="door" then
      switch_room(o[9])
    elseif t~="" then
      obj=o
      objtype=t
      obj_action=2
    end
  elseif mclick & 1 ~=0 then
    local t,o,a = obj_under(mx,my)
    if t~="" and not(t=="way" and (objtype=="lad" or objtype=="pol" or objtype=="con" or objtype=="trp")) then 
      obj=o
      objtype=t
      obj_action=a or 1
    else
      if objtype=="way" then
        _add_obj("way",{1,mx,my},2)
      elseif objtype=="lad" then
        _add_obj("lad",{1,mx,my},2)
      elseif objtype=="pol" then
        _add_obj("pol",{1,mx,my},2)
      elseif objtype=="door" then
        add(laby.door,{roomnb,mx,my,roomnb,mx,my})
        obj=laby.door[#laby.door]
        switch_room(roomnb,"sel_doorexit_init")
      elseif objtype=="bel" and #doors>0 then
        _add_obj("bel",{1,mx,my},2)
      elseif objtype=="loc" and #doors>0 then
        _add_obj("loc",{1,mx,my,1},2)
      elseif objtype=="key" then
        _add_obj("key",{1,mx,my})
      elseif objtype=="lma" and #room.lma<8 and mx>0 then
        _add_obj("lma",{mx,my,0})
      elseif objtype=="swi" then
        _add_obj("swi",{mx,my,0,0},2)
      elseif objtype=="con" then
        _add_obj("con",{1,mx,my,mx+1,my-1,1},2)
      elseif objtype=="frc" then
        _add_obj("frc",{mx,my,mx,my},2)
      elseif objtype=="trp" then
        _add_obj("trp",{mx,my,mx,my,0},2)
      elseif objtype=="tmc" then
        _add_obj("tmc",{1,mx,my})
      elseif objtype=="tmd" and #room.tmd<7 then
        _add_obj("tmd",{#room.tmd+1,mx,my})
      elseif objtype=="mum" then
        _add_obj("mum",{mx,my,mx,my},2)
      elseif objtype=="frk" then
        _add_obj("frk",{mx,my,0})
      elseif objtype=="gun" then
        _add_obj("gun",{1,mx,my,0,-1,-1,0},2)
      end
    end 
      
  -- tool    
  elseif key=="w" then
    objtype="way" obj=nil
  elseif key=="l" then
    objtype="lad" obj=nil
  elseif key=="s" then
    objtype="pol" obj=nil
  elseif key=="d" then
    objtype="door" obj=nil
  elseif key=="b" then
    objtype="bel" obj=nil
  elseif key=="o" then
    objtype="loc" obj=nil
  elseif key=="k" then
    objtype="key" obj=nil
  elseif key=="c" then
    objtype="con" obj=nil 
  elseif key=="m" then
    objtype="lma" obj=nil 
  elseif key=="i" then
    objtype="swi" obj=nil
  elseif key=="f" then
    objtype="frc" obj=nil
  elseif key=="t" then
    objtype="trp" obj=nil
  elseif key=="x" then
    objtype="tmc" obj=nil
  elseif key=="v" then
    objtype="tmd" obj=nil
  elseif key=="u" then
    objtype="mum" obj=nil
  elseif key=="r" then
    objtype="frk" obj=nil
  elseif key=="g" then
    objtype="gun" obj=nil
 
  elseif key==" " then
    local t,o = obj_under(mx,my)
    if t=="door" then
      switch_room(o[9])
    end
  
  elseif key=="+" or key==">" then
    switch_room(roomnb+1)
  elseif key=="-" or key=="<" then
    switch_room(roomnb-1)
  elseif key=="★" then
    -- save
    switch_room(roomnb)
    tape_save_laby()
  elseif key=="➡️" then
    -- rename
    switch_room(roomnb)
    name=laby.name
    state="laby_rename"
  elseif key=="░" or key=="!" then
    -- enter p1 e
    laby.player[1]=roomnb
    laby.player[2]=mx
    laby.player[3]=my
  elseif key=="∧" or key=="\"" or key=="@" then
    -- enter p2 w
    laby.player[1]=roomnb
    laby.player[4]=mx
    laby.player[5]=my
  elseif key=="\9" then
    -- map <tab>
    state="map_edit"
    ret_state="level_init"
  elseif key=="\194" or key=="🐱" then
    -- copy
    switch_room(roomnb)
    tape_doclip=true
    clipstr=""
    tape_write_laby()
    printh(clipstr,"@clip")
    tape_doclip=false
    switch_room(roomnb)
  elseif key=="\213" or key=="ˇ" then
    -- paste
    tape_doclip=true
    clipstr=stat(4)
    tape_read_laby()
    tape_doclip=false
    switch_room(1)
    laby.name="clip"..laby.name
  end
  
end

function update.sel_doorexit_init()
  roomnb = mid(1,roomnb,#laby.room)
  room=laby.room[roomnb]
  if (room==nil) room=new_room()
  laby_draw_map()
  state = "sel_doorexit" 
end

function update.sel_doorexit()
  if my<1 then
    if mxx>=0 and mxx<=8*4 then
      if (mclick & 1 != 0) switch_room(roomnb+1) state="sel_doorexit_init"
      if (mclick & 2 != 0) switch_room(roomnb-1) state="sel_doorexit_init"
    end
    return false
  end
  if mx<0 or mx>15 or
     my<1 or my>15 then
    return false
  end
  
  if mclick&1 !=0 then
    obj[4]=roomnb
    if obj[1]==obj[4] then
      obj[5]=obj[2]
      obj[6]=obj[3]
    else
      obj[5]=mx
      obj[6]=my
    end
    
    switch_room(obj[1])
  
    obj=nil
    --switch change state
  elseif mclick&2 !=0 then
    local t,o = obj_under(mx,my)
    if t=="door" then
      switch_room(o[9],"sel_doorexit_init")
    else
      switch_room(obj[1])
      del(laby.door,obj)
      obj=nil
    end
    --switch change state
  end
  
  if key=="+" or key==">" then
    switch_room(roomnb+1,"sel_doorexit_init")
  elseif key=="-" or key=="<" then
    switch_room(roomnb-1,"sel_doorexit_init")
  elseif key=="\9" then
    state="map_edit"
 ret_state="sel_doorexit_init"
  end
end

function update.laby_rename()
  if key>="a" and key<="z" then
    name..=key
  elseif key=="\08" then
    name=sub(name,1,#name-1)    
  elseif key=="\r" then
    state="level_init"
    if laby.name!=name then
      local o=laby.name
      laby.name=name
      tape_save_laby(o)
    end
  end
  -- prevent pause!
  if(btn(6)) poke(0x5f30,1)
end

function update.menu()
  if (btnp "2") menu_pos -=1
  if (btnp "3") menu_pos +=1
  menu_pos=mid(1,menu_pos,#menu_text)
  
  if (btnp "4" or btnp "5") state=menu_retstate
  
end
function find_room(mx,my)
 local i=1
 for r in all(laby.room) do
   if (r.x<=mx and mx<=r.x+r.w-1 and r.y<=my and my<=r.y+r.h-1) return r,i 
   i+=1
 end   
end
function update.map_edit()
  local r,nb
  if mx<0 or mx>15 or
     my<1 or my>15 then
    return false
  end
  if room_sel==nil then
    r,nb=find_room(mx,my)
    if mclick!=0 then
      room_sel=r
      room_do=mclick
    else      
      if r~=nil then
        if (key>="1" and key<="9") r.col=key-0
        if (key=="0") r.col=10
        if (key=="/") r.col=11
        if (key=="*") r.col=12
        if (key=="," or key==".") r.col=13
        if (key=="\9") switch_room(nb or roomnb,ret_state)
      end
    end
  else
    if room_do&1!=0 then
      room_sel.x,room_sel.y=mx,my
    else
      room_sel.w=mid(mx-room_sel.x+1,1,7)
      room_sel.h=mid(my-room_sel.y+1,1,7)
    end
    if (mclick != 0) room_sel=nil
  end
end
-->8
--draw
function _draw()
  if draw[state]!=nil then
    draw[state]()
  end
end
draw={}

function twodigit(c)
  c="00"..c
  return sub(c,#c-1,#c)
end

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
  draw_topbar(menu_title,12,menu_right)
  print("")
  y=10
  for i=max(menu_pos-9,1),menu_pos-1 do
    print(menu_text[i],1,y,6)
    y+=6
  end
  y+=1
  rectfill(0,y-1,128,y+5,9)
  print(menu_text[menu_pos],2,y,4)
  y+=7
  local i = menu_pos+1
  while y<126 and i<=#menu_text do
    print(menu_text[i],1,y,6)
    y+=6
    i+=1
  end
end

function mypal(x)
  pal(12,colors[x*2-1])
  pal(13,colors[x*2])
end

function draw_map()
  --background transport chamber
  for t in all(room.tmc) do
    boxfill(t[2]*8+3,t[3]*8+1,0xd,0xb,colors[t[1]*2-1])
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
  
  --forcefield
  pal(15, forcecolor[anim_cycle% #forcecolor +1])
  for f in all(room.frc) do
    spr(0x1d,f[1]*8,f[2]*8,1,2)
    spr(0x7a,f[3]*8,f[4]*8+3)
  end
    
  --doors
  for d in all(doors) do
    mypal(d[8])
    spr(0x12,d[d[7]]*8+4,d[d[7]+1]*8+4)
    print(d[10],d[d[7]]*8,d[d[7]+1]*8,7)
    print("r"..d[9],d[d[7]]*8,d[d[7]+1]*8+6,7)
  end
 
  --door bell
  for a in all(room.bel) do
    mypal(doors[a[1]][8])
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
  
  --frankenstein
  for f in all(room.frk) do
    spr(0x44,f[1]*8+(f[3]==1 and 1 or -3),f[2]*8+6,1,1,f[3]==1,false)
  end
  
  --gun
  for g in all(room.gun) do
    spr(0x6a,g[2]*8,(g[3]+g[4])*8,1,1,g[7]==0,false)
    spr(0x69,g[2]*8+(g[7]==0 and 8 or -8),(g[3]+g[4])*8,1,1,g[7]==0,false)
    spr(0x40,g[5]*8,g[6]*8+3)
  end  

  --player
  if roomnb==laby.player[1] then
    spr(0x62,laby.player[2]*8,laby.player[3]*8-3)
    pal(15,4)
    spr(0x62,laby.player[4]*8,laby.player[5]*8-3,1,1,true,false)
    pal()
  end
  
  --lightning
  for m in all(room.lma) do
    spr(0x35,m[1]*8,m[2]*8-3)
    if (m[3]==1) spr(0x6c+rnd(4)\1,m[1]*8,m[2]*8,1,2,rnd(2)>=0.5,false)
  end
  
  --foreground  
  mypal(room.col)
  map(32,1,0,8,16,15,1)--front
  pal()
end

function draw.laby_rename()
  cls()
  local i=anim_cycle%4<2 and 1 or 2
  draw_topbar("rename labyrinth:"..name..sub("#",i,i),1)
  draw_map()
  
end

function draw_grid()
  for x=4,127,8 do
    for y=4,127,8 do
      pset(x,y,1)
    end
  end
end

function draw.level_edit()
  cls()
  draw_grid()
  draw_topbar("room "..twodigit(roomnb).."/"..twodigit(#laby.room).." "..objtype,nil,mx.."x"..my)

  draw_map()
  
  if my>0 then
    if obj==nil then
      local t,o,a = obj_under(mx,my)
      local x1,x2,x3,x4 = obj_box(t,o,a)
      rect(x1,x2,x3,x4,5)
      if t=="con" then
        line(o[2]*8+7,o[3]*8+4,o[4]*8+4,o[5]*8+7,15)
      elseif t=="gun" then
        line(o[2]*8+4,(o[3]+o[4])*8+4,o[5]*8+4,o[6]*8+7,15)
      elseif t=="trp" then
        line(o[1]*8+4,o[2]*8+4,o[3]*8+4,o[4]*8+7,15)
      elseif t=="mum" then
        line(o[1]*8+7,o[2]*8+7,o[3]*8+4,o[4]*8+7,15)
      elseif t=="frc" then
        line(o[1]*8+4,o[2]*8+7,o[3]*8+4,o[4]*8+7,15)
      elseif t=="bel" or t=="loc" then
        local d=doors[o[1]]
        if (d~= nil) line(o[2]*8+4,o[3]*8+7,d[d[7]]*8+7,d[d[7]+1]*8+7,15)
      elseif t=="door" then
        for b in all(room.bel) do 
          if b[1]==o[11] then
            line(b[2]*8+4,b[3]*8+7,o[o[7]]*8+7,o[o[7]+1]*8+7,15)
          end
        end
        for b in all(room.loc) do 
          if b[1]==o[11] then
            line(b[2]*8+4,b[3]*8+7,o[o[7]]*8+7,o[o[7]+1]*8+7,15)
          end
        end
      elseif t=="swi" then
        for m in all(room.lma) do
          if o[4] & (0.5<<m[4]) != 0 then
            line(o[1]*8+4,o[2]*8+4,m[1]*8+3,m[2]*8,12)
          end
        end
      elseif t=="lma" then
        for s in all(room.swi) do
          if s[4] & (0.5<<o[4]) != 0 then
            line(s[1]*8+4,s[2]*8+4,o[1]*8+3,o[2]*8,12)
          end
        end      
      end
    elseif (objtype=="bel" or objtype=="loc") and obj_action==2 then
      line(obj[2]*8+4,obj[3]*8+8,mxx,myy,15)
    elseif objtype=="swi" then
      for m in all(room.lma) do
        if obj[4] & (0.5<<m[4]) != 0 then
          line(obj[1]*8+4,obj[2]*8+4,m[1]*8+3,m[2]*8,12)
        end
      end
      line(obj[1]*8+4,obj[2]*8+8,mxx,myy,15)    
    end
  end
  spr(0x22,mxx,myy)
end

function draw.sel_doorexit()
  cls()
  draw_grid()
  draw_topbar("room "..twodigit(roomnb).."/"..twodigit(#laby.room).." select exit",1)
  
  draw_map()
  
  rect(mx*8,my*8,mx*8+15,my*8+15,8)  
  spr(0x22,mxx,myy)
  
end

function draw.map_edit()
  cls()
  draw_grid()
  draw_topbar("map",2,mx.."x"..my)
  local i=1
  for r in all(laby.room) do
    r.xx,r.yy,r.ww,r.hh=r.x*8,r.y*8,(r.x+r.w)*8-1 , (r.y+r.h)*8-1
    rect(r.x*8,r.y*8,r.ww,r.hh,7)
    
    --rectfill(r.x*8+1,r.y*8+1,w-1,h-1,colors[r.col][1])

    --print(i,r.x*8+2,r.y*8+2,15)  
    --i+=1
    r.cx,r.cy=r.x*8+r.w*4,r.y*8+r.h*4
  end  
  
  function _line(x1,y1,x2,y2,c)
    for x=-1,1 do
      for y=-1,1 do
        line(x1+x,y1+y,x2+x,y2+y,c)
      end
    end
  end
  
  ---[[  
  for d in all(laby.door) do
    local r1,r2,c=laby.room[d[1] ],laby.room[d[4] ]
    if r1!=r2 then
      x1=mid(r1.xx,r1.ww,r2.xx)
      x2=mid(r2.xx,r2.ww,r1.ww)
      y1=mid(r1.yy,r1.hh,r2.yy)
      y2=mid(r2.yy,r2.hh,r1.hh)
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

 ---[[
  for r in all(laby.room) do
    rectfill(r.x*8+1,r.y*8+1,r.ww-1,r.hh-1,colors[r.col*2-1])
    r.hi=r.xx<mxx and mxx<=r.ww and r.yy<myy and myy<=r.hh
  end
  --]]  

  for d in all(laby.door) do
    local r1,r2=laby.room[d[1] ],laby.room[d[4] ]
    if (r1.hi or r2.hi) line(r1.cx,r1.cy,r2.cx,r2.cy,1)
  end 
 
  x=16
  for r in all(laby.room) do
    for a in all(r.loc) do
      mypal(a[4])
      spr(0x59,x,r.hi and 2 or 0) 
      x+=4
    end
    pal()
    for a in all(r.key) do
      spr(0x25+a[1],x,r.hi and 2 or 0)
      x+=4
    end
    print(i,r.x*8+2,r.y*8+2,colors[r.col*2])  
    i+=1
  end
 
  
  spr(0x22,mxx,myy)
end

__gfx__
00000000000000000000000000000000005005000006500000000088820000000000009a94000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000555500000650000008208222082000000099a999440000000000000000000000000000000ccc000820888208882082
000000000000000000000000000000000050050000065000000820000008200000999aa99994440000000000000099900000000000cccd000820822208222082
00000000000000000000000000000000005555000006500000000000000000009999a7a9999944440000444000099900000000000cccd0000000000000000000
0000000011000000cccccccc00000000005005000006500008820000000088200a9400000000a94000004440009990000000f0000ddd00000888208882088820
0000000010000000cccccccc0000000d005555000006500008220000000082200a9400000000a9400000444009990000000ff000000000000822208222082220
0000000000000000cccccccc000000d1005005000006500000000000000000000a9400000000a940000044400aaa000000fff000000000000000000000000000
0000000000000000dddddddd00000011005555000006500000820000000082000a9400000000a940000044400aaa000000fff000000000000820888208882082
00066666666666660000000000000000000000000000000000820000000082000a9400000000a940000044400aaa000000fff000005555000820822208222082
00666666666666600000000307600763000000000000000000000000000000000a9400000000a940000044400aaa000000fff000055665500000000000000000
0666666666666600000000bb67766776000000000000000088820000000088820a9400000000a940000044400aaa000000fff000056655500888208882088820
77777777777770000000033307600763000000000000000082220000000082220a9400000000a940000044400aaa000000fff000056555500822208222082220
77777777777770000000bbbb67766776ccccccccc000000000000000000000000000000000000000000000000aaa000000fff000055555500000000000000000
77777000777770000003333307633763cdddddddd000000c00000000000000000000000000000000000000000aaa000000ff0000005555000000000000000000
777000000077700000bbbbbb67766776d000000ccccccccc00000000000000000000000000000000000000000aaa000000f00000000000000000000000000000
770000000007700003333333076337630000000ddddddddd00000000000000000000000000000000000000000000000000000000000000000000000000000000
77000000000770000100000000555100005005000006500000000000000000000000000000000000000000000000000000000000002222000000000000000000
7700000000077000171000000510051000555500000650003bb3000056500000288200002ee20000aaa00000dcd0000024420000028888800000000000000000
777000000077700017710000051005100050050000065000b0bb00006060000082080000e00e0002a9a00000cdcd00dc40040000280000280000000000000000
777770007777700017771000005551000055550000065000b0bbbbbb6066666688028888e00eeeeea9aaaaaac0dccccc40044444280000280000000000000000
777777777777700017710000055511100050050000065000bbbb00b36060065682080008e00e002eaaa00a0acdcd000040040004280000280000000000000000
7777777777777000171000000005100000000000000000003bb3000056500000288200002ee20000aaa00000dcd0000024420000280000280000000000000000
77777777777770000100000000051000000000000000000000000000000000000000000000000000000000000000000000000000082222800000000000000000
00000000000000000000000000051000000000000000000000000000000000000000000000000000000000000000000000000000008888000000000000000000
00000000000000000000000000000000003b33000000000000000000000000000000000000000000000000000000000000000000000000000008800000055000
00000000000000000000000000000000003b33000000000000000000000000000000000000000000000000000000000000000000000000000088820000555100
00000000000000000000000000000000003b33000000000000000000000000000000000000000000000000000000000000000000000000000088220000551100
00000000000000000000000000000000003b33000000000000000000000000000000000000000000000000000000000000000000000000000002200000011000
cc000000ccddddddcccccccccccccccc003b330000000000ccd0ccd0ccd0ccd0cc0ccd0ccd0ccd0cccccd0ccd0ccd0cccccd0ccd0ccd0ccd00055000000bb000
c000000cc000000ccddddddccccccccc003b330000000000cd0ccd0ccd0ccd0cc0ccd0ccd0ccd0cccccd0ccd0ccd0cccccd0ccd0ccd0ccdc0055510000bbb300
000000cc000000cc000000ccddddddcc003b330000000000d0ccd0ccd0ccd0cc0ccd0ccd0ccd0cccccd0ccd0ccd0cccccd0ccd0ccd0ccdcc0055110000bb3300
000000dd000000dd000000dd000000dd003b330000000000dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd0001100000033000
00051000000b30000005100000051000000bb000000bb000000bb000000bb000000bb00000076000000000001670000000000761000000000000000000000000
0055110000bb33000055110000551100000bb000000bb000000bb000000bb000000bb0000aa76aa00aaaaaa016000000000000610050050000b0050000500800
055551100bbbb33005555110055551100000b0000000b0000000b0000000b0000000b0000a0760a00a0000a01670000000000761055005500bb0055005500880
0005100000051000000510000009400000033000003333000033330000333300003333000a7666a00a0000a0160000000000006155155551bb35555155155882
0005100000051000000510000009400000033000030330300303303003033030030330300a0000a00a7666a0167000000000076155111551bb31155155111882
0555511005555110088882200555511000088000000882000028800000088800008880000a0000a00a0760a01600000000000061011005100330051001100820
0055110000551100008822000055110000082000008002000020800000200800008020000aaaaaa00aa76aa01670000000000761001001000030010000100200
00051000000510000008200000051000000820000080000000008000002000000000200000000000000760001600000000000061000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000dcd100003bbb300056665000288820002eee20009aaa9000dcccd000244420
000bb0000006600000088000000ee000000aa000000cc000000440000000000000dcd10000bb0bb0006606600088088000ee0ee000aa0aa000cc0cc000440440
00b33b00006556000082280000e22e0000a99a0000cddc000042240000011000006cd50000b000b0006000600080008000e000e000a000a000c000c000400040
0b376b3006576650082768200e276e200a976a900cd76cd0042764200017601000d7610000b000b0006000600080008000e000e000a000a000c000c000400040
0b366b3006566650082668200e266e200a966a900cd66cd0042664200016601000dcd10000bb0bb0006606600088088000ee0ee000aa0aa000cc0cc000440440
00bbb300006665000088820000eee20000aaa90000cccd00004442000000010000dcd10000b000b0006000600080008000e000e000a000a000c000c000400040
0003300000055000000220000002200000099000000dd0000002200000011000006cd50000b000b0006000600080008000e000e000a000a000c000c000400040
000000000000000000000000000000000000000000000000000000000000000000d76100003bbb300056665000288820002eee20009aaa9000dcccd000244420
000ff000000ff00000ff0000000ff000000000000007700000770000000770000077000000000880005650000065600000000000000000000000000000000700
000ff000000ff00000ff0000000ff000000000000007700000770000000770000077000000000228062826000528200000700700000070000000007000007000
0000f0000000f000000f00000000f000000000000000700000070000000070000007000000000288058180000681860000700700000070000000070000007000
0009a0000009a000000a94000009a000667770770777700007777000077770000777700000077788c68c8600c58c800000700070000007000700070000070000
00499a000009a00000a990400009a000777777770067700000677000006770000067700000000288058180000681860007000070000007000700770000070000
000cc00000dcc000000cc00000ccc000000000000007700000077000000770000007700000000288062826000528200007000007000077007000707000707700
00c0dd00000dc00000d0cc00000cd000000000000077600000076000006670000006700000000880005650000065600007000007000707007700000700700077
00c000000000c00000d000000000d000000000000000600000076000000070000006700000000000000000000000000000700007007000707070000707070070
000ff000000ff000000ff000000ff000000ff0000000000000000000000000000000000000000000000000000000000000700070077000700070007000000700
000ff000000ff000000ff000000ff000000ff000000bb0000006600000088000000ee000000aa000000cc0000004400000770070007000000077007000000700
0000f4000000f0000040f0000000f0000090f90000bbbb00006666000088880000eeee0000aaaa0000cccc000044440000700070000700000700007000007000
00999900049994000099990000499940009999000bbbbb3006666650088888200eeeee200aaaaa900cccccd00444442007000707000700000700007000007000
00499000000990000009940000099000000990000bbbbb3006666650088888200eeeee200aaaaa900cccccd00444442007000707000700007000070000007000
000cc000000cc000000cc000000cc000000cc00000bbb300006665000088820000eee20000aaa90000cccd000044420007000700000000007000070000070000
000cd000000cd000000cd000000cd00000c00d000003300000055000000220000002200000099000000dd0000002200000000000000000000000000000070000
000c00000000d0000000d000000c0000000cd0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
08208882088820820000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
08208222082220820000000000000000000000000000000000000000000000000000000000000000000099900000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000999f00000000000000000000000000000000000000000
0882222222228820000000000000000000000000000000000000000000000000000000000000000000999ff00000000000000000000000000000000000000000
082200044444822000000000000000000000000000000000000000000000000000000000000000000999fff00000000000000000000000000000000000000000
000200444444800000000000000000000000000000000000000000000000000000000000000000000aaafff00000000000000000000000000000000000000000
082204444444808200000000000000000000000000000000000000000000000000000000000000000aaafff00000000000000000000000000000000000000000
082288888888808200222200002222000022220000222200002222000022220000222200002222000aaafff00000000000000000000000000000000000000000
000000000000000002888880028888800288888002888880028888800288888002888880028888800aaafff00000000000000000000000000000000000000000
088820888208882028000028280000282800002828000028280000282800002828155528286666280aaafff00000000000000000000000000000000000000000
082220822208222028000028280000282800002828000028285666282867772828677728286777280aaafff00000000000000000000000000000000000000000
cccccccccccccccc28000028280000282856662828677728286777282867772828677728286777280aaaff000000000000000000000000000000000000000000
cccccccccccccccc28566628286777282867772828677728286777282867772828677728286777280aaaf0000000000000000000000000000000000000000000
cccccccccccccccc08222280082222800822228008222280082222800822228008222280082222800aaa00000000000000000000000000000000000000000000
dddddddddddddddd0088880000888800008888000088880000888800008888000088880000888800000000000000000000000000000000000000000000000000
0ac52e8b14ba042aae642be6e02fee613f87e50042b25cc5f66ac84b29ced49953750e316a06e14ddae45dd2657eca610c7ee3b7600a2e2f2674cb688ca7d64d
e4d9517b82fd6407203ea3893a4a3da663038e910d4d108e85588405488c8ff882cee8a660dd580a868b76640b663c4b2642443811aa8a7f70af208ed8528459
52c431624df877b8daac2100c3009e2ba996891cc8423c70840c544288cca00124d5224a58e4e804d0ecb99f85ae9ab724d1350b67ec2fd4cca499155badc20b
07100e602aacce3aa22f3aa88f9281b36c2264e45c7386561856aa52c6d407355534312507e4cc9d8923eaa203630fae6f4e6004d03cedd73cc1521ce9956ee5
2711d45019567021d893295ab30054018898b3d03f6760204b2b3c20e3ed9599f57e6c8559410e0adaf2e2ee3e36614dc24cb1a14bb3c14bb5e14bc7088a87cf
808f809871025cbfac9bcdea532119350c620b040e504b8043008d7e2a9d248d708da13882a468b2234bba26366aa516e89e08a871116173c3aaccb25003a473
010375a21573a03e7c6178624a0ed969250da5bc312848502d4d559f2006f005a33668a6c632384643cb74130d1332bdd44ee4dcf538ba1a6abe865ba4cbede2
7fb402c001a04590a88017fcb7850d4a7d00a51002a21662921baa4bc98e1e7b8aa77da99162619b29d9daad618b196445b2c46592a7eb47abcc0a8205b08511
8b146180e880a3a67a93b6d85e86cf4b9141b34c83c9db00722c25d00f8a68c5e22ef3c08c1816aef01310b4c5a432f47080bc1c22631144f84a264290c19fc1
1c607405242944084e1f9091d326a4f407784922e452078a10f6f9bac961490880b4835985c6562ab835aa815717887808200b0930c7a21279b08bf83c230285
a738cc04257d21883d1c2c230f84d84815329e81982074e3740314194d014a190293c32464f4852939cd125e803998c5a200b57e0c125e80399885a200b57e0c
00000000000000000000000000000000000000000000000016000000000000610000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000016700000000007610000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000016000000000000610000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000016700000000007610000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000016000000000000610000000000000000000000000000000000000000000000000000000000000000
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
__label__
99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
97779777977799999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999777979797779
97779797979799999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999799979797979
97979777977799999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999777997997779
97979797979999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999997979799979
97979797979999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999777979799979
99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000777777777777777700000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000007bbbbbbbbbbbbbb700000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000007b7b7bbbbbbbbbb700000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000007b7b7bbbbbbbbbb700000000000000000000000000000000000000000000000000000000000000000000000000000000
000010000000100000001000000010007b777bbbbbbbbbb700001000000010000000100000001000000010000000100000001000000010000000100000001000
000000000000000000000000000000007bbb7bbbbbbbbbb700000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000007bbb7bbbbbbbbbb700000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000007bbbbbbbbbbbbbb700000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000007bbbbbbbbbbbbbb700000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000007bbbbbbbbb1bbbb700000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000007bbbbbbbb1e1bbb700000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000007bbbbbbbb1ee1bb700000000000000000000000000000000000000000000000000000000000000000000000000000000
000010000000100000001000000010007bbbbbbbb1eee1b700001000000010000000100000001000000010000000100000001000000010000000100000001000
000000000000000000000000000000007bbbbbbbb1ee1bb700000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000007bbbbbbbb1e1bbb700000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000770007777717777700000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000007777777777777777770077777777777777777777777777777777777777777777777777777777777777777777777777770000000000000000
00000000000000007dddddddddddddddddddddd77cccccccccccccccccccccccccccccccccccccccccccccc77dddddddddddddddddddddd70000000000000000
00000000000000007d77ddddddddddddddddddd77c777cccccccccccccccccccccccccccccccccccccccccc77d777dddddddddddddddddd70000000000000000
00000000000000007dd7ddddddddddddddddddd77ccc7cccccccccccccccccccccccccccccccccccccccccc77ddd7dddddddddddddddddd70000000000000000
00001000000010007dd7ddddddddddddddddddd77c777cccccccccccccccccccccccccccccccccccccccccc77dd77dddddddddddddddddd70000100000001000
00000000000000007dd7ddddddddddddddddddd77c7cccccccccccccccccccccccccccccccccccccccccccc77ddd7dddddddddddddddddd70000000000000000
00000000000000007d777dddddddddddddddddd77c777cccccccccccccccccccccccccccccccccccccccccc77d777dddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd77cccccccccccccccccccccccccccccccccccccccccccccc77dddddddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd77cccccccccccccccccccccccccccccccccccccccccccccc77dddddddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd77cccccccccccccccccccccccccccccccccccccccccccccc77dddddddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd00cccccccccccccccccccccccccccccccccccccccccccccc00dddddddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd00cccccccccccccccccccccccccccccccccccccccccccccc00dddddddddddddddddddddd70000000000000000
00001000000010007dddddddddddddddddddddd77cccccccccccccccccccccccccccccccccccccccccccccc77dddddddddddddddddddddd70000100000001000
00000000000000007dddddddddddddddddddddd77cccccccccccccccccccccccccccccccccccccccccccccc77dddddddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd77cccccccccccccccccccccccccccccccccccccccccccccc77dddddddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd77777777777777777777777777777777777777777777777777dddddddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd70000000000000000000000000000000000000000000000007dddddddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd70000000000000000000000000000000000000000000000007dddddddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd70000000000000000000000000000000000000000000000007dddddddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd70000000000000000000000000000000000000000000000007dddddddddddddddddddddd70000000000000000
00001000000010007dddddddddddddddddddddd70000100000001000000010000000100000001000000010007dddddddddddddddddddddd70000100000001000
00000000000000007dddddddddddddddddddddd70000000000000000000000000000000000000000000000007dddddddddddddddddddddd70000000000000000
00000000000000007dddddddddddddddddddddd70000000000000000000000000000000000000000000000007dddddddddddddddddddddd70000000000000000
00000000000000007777777777777777777777770000000000000000000000000000000000000000000000007777777777777777777777770000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000000010000000100000001000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

__gff__
0408010801010101010101012001010101012020010101010101010120010101010120020808020202020202020200000101010104000101010101010101020202020202202020202002020101020202020202020202020204020202020202022020202020202020202020202020202020202020200404040404040420202020
0000000000000000000001000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
