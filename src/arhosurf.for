*******************************************
*  calculation of the arhos coefficient   *
*******************************************

	  subroutine arhosurf(key,rhovs2,epg1,rhov1,dx,dt,mdot2,
     *              ds,epb1,rho_b,epss,rho_potash,w1,rhoa1,epb2,
     *                    w2,epg2,rhoa2,s_c,ddx,n,arhos)

	  implicit doubleprecision (a-h,o-z)

	  integer n
	  integer key(n)
	  doubleprecision epg1(n),epg2(n),epss,epb1(n),epb2(n)
	  doubleprecision rhoa1(n),rhoa2(n),s_c(n),w1(n),w2(n)
      doubleprecision rhovs2(n),rhov1(n)
	  doubleprecision mdot2(n),arhos(n),dx(n)
	  doubleprecision dt,ds,rho_potash,rho_b,ddx

c
c
	  if (key(1).eq.0) then

	  arhos(1)=s_c(1)+(ds+ddx)/dt*((epb1(1)*rho_b+epss
     *           *rho_potash
     *           *w1(1)+epg1(1)*rhov1(1))-(epb2(1)*rho_b
     *           +epss*rho_potash*w2(1)))

	  else

	  arhos(1)=rhovs2(1)*0.90
		
	  end if

	  do i=2,n-1
	  if (key(i).eq.0) then
	    arhos(i)=epg1(i)*rhov1(i)*dx(i)/dt+s_c(i)*dx(i)
	  else
	    arhos(i)=rhovs2(i)*0.80          
	  end if

	  end do

	  if (key(n).eq.0) then

	    arhos(n)=epg1(n)*rhov1(n)*ddx/dt+s_c(n)*ddx

	   else
	    arhos(n)=rhovs2(n)*0.80
	   end if


	  return
	  end