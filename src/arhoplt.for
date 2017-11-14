*******************************************
*   calculation of the arhop coefficient  *
*******************************************

	  subroutine arhoplt(key,n,arhow,arhoe,s_p,epg2,dx,dt,
     *                         ds,ddx,arhop)
	  implicit doubleprecision (a-h,o-z)
	  integer n
	  integer key(n)
	  doubleprecision arhow(n),arhoe(n),arhop(n),s_p(n)
	  doubleprecision epg2(n),dx(n)
	  doubleprecision ddx,dt,ds

	
	  if(key(1).eq.0) then
	     arhop(1)=arhoe(1)+s_p(1)+(ds+ddx)*epg2(1)/dt
	  else
	     arhop(1)=1
	  end if
	
	  do i=2,n-1
	  if(key(i).eq.0) then
	   arhop(i)=arhoe(i)+arhow(i)+epg2(i)*dx(i)/dt-s_p(i)*dx(i)
	  else
	   arhop(i)=1
	  end if

	  end do
	  if (key(n).eq.0) then
	     arhop(n)=arhow(n)+epg2(n)*ddx/dt-s_p(n)*ddx
	  else
	     arhop(n)=1
	   end if
	  return
	  end