**************************
*   residue check up     *
**************************


	subroutine residual(n,arhow,arhoe,arhop,arhos,rhov3,
     *                       residue,r_max,r_sum,mp)

      implicit doubleprecision(a-h,o-z)
	integer n,mp
	doubleprecision arhow(n),arhoe(n),arhop(n),arhos(n)
	doubleprecision r_max,r_sum,rhov3(n),residue(n)

	residue(1)=abs(arhop(1)*rhov3(1)-arhoe(1)*rhov3(2)-
     *             arhos(1))

	do i=2,n-1

	residue(i)=abs(arhop(i)*rhov3(i)-arhow(i)*rhov3(i-1)-
     *             arhoe(i)*rhov3(i+1)-arhos(n))
	end do
 	residue(n)=abs(arhop(n)*rhov3(n)-arhow(n)*rhov3(n-1)-
     *              arhos(n))

       	r_max=	residue(n)
		mp	 =	n

	do i=1,n-1
	
	  if(residue(i).gt.r_max) then 

	     r_max=residue(i)
             m_p=i
	  end if

	end do

	r_sum=0
	do i=1,n
	 r_sum=r_sum+residue(i)
	end do

 	r_sum=r_sum/float(n)
	

	return
	end