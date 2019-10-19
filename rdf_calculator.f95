program HISTORY_FILE

use center 
use rdf

implicit none

! IMPORTANT NOTE BELOW
! THIS PROGRAM WILL read AND CALCULATE RADIAL DISTRIBUTION FUNCTION FROM DL_POLY OUTPUT NAMED AS HISTORY.txt FILE

! VARIABLES AND PARAMETER DECLARATIONS---------------------------------------------------

CHARACTER(3) :: atom

integer, parameter :: tot_config = 2500
integer, parameter :: tot_atom_count = 340 ! Total number of atoms in the system

integer, parameter :: p_hcw = 40, p_cw = 40, p_hcr = 20, p_cr = 20
integer, parameter :: p_cl = 20, p_na = 40, p_h1 = 120

integer i,j,k,p,col1,init_config,final_config,con_range
integer na_c,cw_c,cr_c,hcr_c,h1_c,hcw_c,cl_c

real, parameter :: c = 8.752448, dr = 0.1

real, dimension(1:p_cw) :: cwx,cwy,cwz
real, dimension(1:p_na) :: nax,nay,naz
real, dimension(1:p_cr) :: rc1,rc2,rc3
real, dimension(1:p_cr) :: crx,cry,crz
real, dimension(1:p_hcw) :: hcwx,hcwy,hcwz
real, dimension(1:p_hcr) :: hcrx,hcry,hcrz
real, dimension(1:p_h1) :: h1x,h1y,h1z
real, dimension(1:p_cl) :: clx,cly,clz

real, dimension(1:int(c/dr)) :: rdf_hcr, rdf_hcw, rdf_h1, rdf_rc
real, dimension(1:int(c/dr)) :: tot_rad_func_hcr, tot_rad_func_hcw, tot_rad_func_h1,tot_rad_func_rc
real col2,col3
! PARAMETERS DECLARED-------------------------------------------------------------------

na_c=1; cw_c=1; cr_c=1; hcr_c=1; h1_c=1; hcw_c=1; cl_c=1
tot_rad_func_hcr = 0; tot_rad_func_hcw = 0; tot_rad_func_h1 = 0; tot_rad_func_rc = 0

! FILE WITH UNIT 1 IS INPUT FILE AND SHOULD BE READ BY THE PROGRAM----------------------
open(unit =1, file = 'HISTORY.txt', status = 'UNKNOWN', action ='read')

open(unit =2, file = 'hcr_cl.txt', status = 'UNKNOWN', action ='write')
open(unit =3, file = 'h1_cl.txt', status = 'UNKNOWN', action ='write')
open(unit =4, file = 'hcw_cl.txt', status = 'UNKNOWN', action ='write')
open(unit =10, file = 'rc_cl.txt', status = 'UNKNOWN', action ='write')
!  -------------------------------------------------------------------------------------

write(*,*) 'Please note the instruction shown just below\n'
write(*,*) 'Enter integers like 400,900'
write(*,*) 'Integers should be greater than 1 inclusive and lesser than 2500 exclusive'
read(*,*) init_config, final_config
write(*,*) ' '
write(*,*) 'You entered following values,', init_config, final_config
con_range = final_config - init_config
write(*,*) 'The range of configurations to be calculated is,', (con_range+1)

! FIRST SIX LINES ARE SKIPPED BELOW
do i = 1, 6
read(1,*)
end do
!  ----------------------------------------

!SKIP TO THE INITIAL CONFIGURATION
do j = 1, (init_config - 1)*684
read(1,*)
end do
!--------------------------------------

!AFTER SKIPPING TO THE INITIAL CONFIGURATION WE START SPECIES IDENTIFICATION AND
!THEN RECORD ITS TRAJECTORY/CO-ORDINATES
do  i = init_config, final_config

do j = 1, tot_atom_count

        read (1,*) atom, col1,col2,col3

        if (atom == 'NA') then
            read (1,*) nax(na_c),nay(na_c),naz(na_c)
            na_c = na_c + 1
            cycle

        else if (atom == 'CR') then
            read (1,*) crx(cr_c),cry(cr_c),crz(cr_c)
            cr_c = cr_c + 1
            cycle

        else if (atom == 'CW') then
            read (1,*) cwx(cw_c),cwy(cw_c),cwz(cw_c)
            cw_c = cw_c + 1
            cycle

        else if (atom == 'C1') then
            read (1,*) ! SKIP THE DATA OF C1
            cycle

        else if (atom == 'HCR') then
            read (1,*) hcrx(hcr_c),hcry(hcr_c),hcrz(hcr_c)
            hcr_c = hcr_c + 1
            cycle

        else if (atom == 'HCW') then
            read (1,*) hcwx(hcw_c),hcwy(hcw_c),hcwz(hcw_c)
            hcw_c = hcw_c + 1
            cycle

        else if (atom == 'H1') then
            read (1,*) h1x(h1_c),h1y(h1_c),h1z(h1_c)
            h1_c = h1_c + 1
            cycle

        else if (atom == 'Cl') then
            read (1,*) clx(cl_c),cly(cl_c),clz(cl_c)
            cl_c = cl_c + 1
            cycle

        end if

end do
        ! END OF A CONFIGURATION
        na_c=1; cw_c=1; cr_c=1; hcr_c=1; h1_c=1; hcw_c=1; cl_c=1

        !SKIP THE FOUR LINES AFTER EACH CONFIGURATION
        do k = 1, 4
        read(1,*)
        end do

! THE CALCULATION OF CENTER OF RING
 call  rcent(crx,cry,crz,nax,nay,naz,cwx,cwy,cwz,rc1,rc2,rc3)
! THE RING CENTER CO-ORDINATES IS WRITTEN IN ARRAY ''rc1,rc2,rc3''
!----------------------------------------------------------------

! THE RADIAL DENSITIES OF SPECIES "$$_-SUBROUTINE-_$$ ON THE RUN"
 call radfunc(hcrx,hcry,hcrz,p_hcr,clx,cly,clz,rdf_hcr)
tot_rad_func_hcr = tot_rad_func_hcr + rdf_hcr

 call radfunc(h1x,h1y,h1z,p_h1,clx,cly,clz,rdf_h1)
tot_rad_func_h1 = tot_rad_func_h1 + rdf_h1

 call radfunc(hcwx,hcwy,hcwz,p_hcw,clx,cly,clz,rdf_hcw)
tot_rad_func_hcw = tot_rad_func_hcw + rdf_hcw

 call radfunc(rc1,rc2,rc3,p_cr,clx,cly,clz,rdf_rc)
tot_rad_func_rc = tot_rad_func_rc + rdf_rc

! THIS IS THE TESTING SITE OF THIS PROGRAM CODE
!     print*, i,j
!     do p = 1, int(c/dr)
!     print*,  tot_rad_func_rc(p) !rc1(p), rc2(p), rc3(p)
!     end do
!     if (i .eq. 500) then
!     STOP
!     end if
!     print*, con_range
!-------------------------------------------------------------

!--------------------------------------------------------------

! THE LOOP OVER THE COFIGURATIONS ENDS WITH FOLLOWING "END DO" SYNTAX
end do
!------------------------------------------------------------

! CALCULATE THE AVERAGE OF RDF OVER THE RANGE OF CONFIGURATIONS
tot_rad_func_hcr = tot_rad_func_hcr/con_range; tot_rad_func_h1 = tot_rad_func_h1/con_range
tot_rad_func_hcw = tot_rad_func_hcw/con_range; tot_rad_func_rc = tot_rad_func_rc/con_range
!------------------------------------------------------------

! WRITE THE DATA INTO A *.txt FILE
do i = 1, int(c/dr)
    write(2,*) i*dr, tot_rad_func_hcr(i)
    write(3,*) i*dr, tot_rad_func_h1(i)
    write(4,*) i*dr, tot_rad_func_hcw(i)
    write(10,*) i*dr, tot_rad_func_rc(i)
end do
!------------------------------------------------------------

close(1); close(2); close(3)
close(4); close(10)

write(*,*) ' '
write(*,*) 'THE COMPUTATIONS HAVE BEEN COMPLETED, PLEASE CHECK FOR TEXT(*.txt) FILES'
write(*,*) 'IN THE CURRENT DIRECTORY IN ORDER TO PLOT THE RESULTS'

end program HISTORY_FILE
