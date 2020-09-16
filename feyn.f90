program feynman
implicit none

integer::i,n,q1,n1,n2
real::x,y,xh,yh,thetarad1,theta1,thetarad2,theta2,d1,d2,t1,t2,dt,t,tf,vx1,vx2,vy2,vy1
real,parameter::pi=acos(-1.0),xor=0.0,yor=0.0,t0=0.0,x0=-10.0,y0=10.0,xf=10.0,yf=15.0,v=2.0

print*,"hurrrrrrrr"

print*,"give the hitting point's coordinate(y=0.0 mandatory for this prog)"
read*,xh,yh



print*,"give dt"
read*,dt

d1=sqrt((xh-x0)**2+(yh-y0)**2)
t1=d1/v
d2=sqrt((xf-xh)**2+(yf-yh)**2)
t2=d2/v
tf=(t1+t2)

n=int((tf-t0)/dt)
n1=int((t1-t0)/dt)

x=x0
y=y0
t=t0
thetarad1=atan(abs((y0-yh)/(x0-xh)))
thetarad2=atan(abs((yf-yh)/(xf-xh)))
vx1=v*cos(thetarad1)
vy1=-v*sin(thetarad1)
vx2=v*cos(thetarad2)
vy2=v*sin(thetarad2)

print*,t1,d2,t2,tf,n1,n,vy1
!=====================================================
open(1,file="action20.dat")
write(1,*)x,y,vx1,vy1,t
do i=0,n
t=i*dt+t0
if(i<=n1)then
x=vx1*dt+x
y=vy1*dt+y
else if(i>n1)then
x=vx2*dt+x
y=vy2*dt+y
end if
write(1,*)x,y,vx1,vy1,t
end do

end program
