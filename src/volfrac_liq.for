******************************************************
*  calculation of the liquid phase volume fraction   *
******************************************************

	  subroutine volfrac_liq(epb_1,mdot,dt,epb_3)
	  implicit doubleprecision (a-h,o-z)
	  doubleprecision epb_1,epb_3,mdot,dt
	
	  epb_3=epb_1-mdot*(1/1000)*dt
	
      return
	  end