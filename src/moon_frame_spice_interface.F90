!***************************************************************************
!>
!  Interface to SPICE library for Moon frame transformations

module moon_frame_spice_interface

    use iso_fortran_env, only: wp => real64 !! spice uses double precision

    implicit none

    private

    ! SPICE IDs
    integer,parameter,public :: j2000_id = 1
    integer,parameter,public :: iau_moon_id = 10020
    integer,parameter,public :: moon_me_id = 31001 !! from the kernel
    integer,parameter,public :: moon_pa_id = 31000 !! from the kernel

#ifdef HAS_SPICELIB

    ! from spicelib:
    interface
        subroutine refchg ( frame1, frame2, et, rotate )
            import
            implicit none
            integer :: frame1
            integer :: frame2
            real(wp) :: et
            real(wp) :: rotate ( 3, 3 )
        end subroutine refchg
        subroutine furnsh ( file )
            import
            implicit none
            character(len=*) :: file
        end subroutine furnsh
        subroutine str2et ( timstr, et )
            import
            implicit none
            character(len=*) :: timstr
            real(wp) :: et
        end subroutine str2et
    end interface
    public :: refchg, furnsh, str2et
    public :: from_j2000_to_iau_moon, from_j2000_to_moon_me, from_j2000_to_moon_pa

    contains

    subroutine from_j2000_to_moon_me(et, rot)
        real(wp), intent(in) :: et
        real(wp), intent(out) :: rot(3, 3)
        call refchg ( j2000_id, moon_me_id, et, rot )
    end subroutine from_j2000_to_moon_me

    subroutine from_j2000_to_moon_pa(et, rot)
        real(wp), intent(in) :: et
        real(wp), intent(out) :: rot(3, 3)
        call refchg ( j2000_id, moon_pa_id, et, rot )
    end subroutine from_j2000_to_moon_pa

    subroutine from_j2000_to_iau_moon(et, rot)
        real(wp), intent(in) :: et
        real(wp), intent(out) :: rot(3, 3)
        call refchg ( j2000_id, iau_moon_id, et, rot )
    end subroutine from_j2000_to_iau_moon

#endif

end module moon_frame_spice_interface