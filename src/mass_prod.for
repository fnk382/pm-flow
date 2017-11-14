*******************************************************************
*  calculation of the rate of phase change of  water              *
*				vapor mass (kg/m3.s)                            *
*******************************************************************

	  subroutine mass_prod(sv,phiv,xm,xc,rho_potash,dphidt,dmdt,mdot)
	  implicit doubleprecision (a-h,o-z)
	  doubleprecision phiv,xm,xc,rho_potash
	  doubleprecision x1,x2,x3
	  doubleprecision x4,x5,x6,x7
	  doubleprecision sv,std
	  doubleprecision dphidt,dmdt,mdot

	  x1=4.18d-01
	  x2=-3.846d-01
	  x3=8.94d-02

	  x4=7.646397d+03
	  x5=-2.8984243d+04
	  x6=3.7082123d+04
	  x7=-1.596131d+04
	
	  std=4.10d+03
	
	  if (phiv.le.4.85d-01) then	
	    dmdt=((1-phiv)*(1+(xc-1)*phiv)-phiv*(2*phiv-2*xc*phiv+
     *            xc-2))/((1-phiv)*(1+(xc-1)*phiv))**2*xc*xm*
     *            dphidt*rho_potash
	  else if (phiv.gt.4.85d-01.and.phiv.lt.5.3d-01) then
	    dmdt=(2*x1*phiv+x2)*dphidt*rho_potash
	  else
	    dmdt=(-x5-2*x6*phiv-3*x7*phiv**2)/(x4+x5*phiv+x6*phiv**2
     *            +x7*phiv**3)**2*dphidt*rho_potash
	end if
	   

	mdot=-1*dmdt

	return
	end