**********************************************
*    calculation of the arhoe coefficient    *
**********************************************

	subroutine arhoew(key,n,deffe,deffw,dxw,dxe,arhow,arhoe)

	implicit doubleprecision(a-h,o-z)
	integer n
	integer key(n)
	doubleprecision deffe(n),dxe(n),arhoe(n)
	doubleprecision deffw(n),dxw(n),arhow(n)

	do i=2,n-2
      if(key(i).eq.0) then
	      arhoe(i)=deffe(i)/dxe(i)
	else

	arhoe(i)=0
	end if 	
	end do
	  if(key(1).eq.0) then
	   arhoe(1)=deffe(1)/dxe(1)
	  else
	  arhoe(1)=0
	  end if
		  if(key(n-1).eq.0) then
	  arhoe(n-1)=deffe(n-1)/dxe(n-1)
		  else
		     arhoe(n-1)=0
		  end if
	  arhoe(n)=0

c	Calculating arhow
	
	  do i=3,n-1
	   if(key(i).eq.0) then
	      arhow(i)=deffw(i)/dxw(i)
	   else
	      arhow(i)=0
	  end if
	  end do
	   arhow(1)=0
           if (key(2).eq.0) then
   	      arhow(2)=deffw(2)/dxw(2)
	   else
	      arhow(2)=0
	   end if
		  if (key(n).eq.0) then
		      arhow(n)=deffw(n)/dxw(n)
 	   else
	      arhow(n)=0
	  end if
	  return
	  end