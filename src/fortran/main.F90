module BabelStreamUtil
    use, intrinsic :: ISO_Fortran_env
    use BabelStreamTypes

    implicit none

    integer(kind=StreamIntKind) :: array_size = 33554432
    integer(kind=StreamIntKind) :: num_times  = 100
    logical                     :: mibibytes  = .false.
    logical                     :: csv        = .false.

    ! 1 = All
    ! 2 = Triad
    ! 3 = Nstream
    integer                     :: selection  = 1

    real(kind=REAL64), parameter :: startA      = real(0.1d0,kind=REAL64)
    real(kind=REAL64), parameter :: startB      = real(0.2d0,kind=REAL64)
    real(kind=REAL64), parameter :: startC      = real(0.0d0,kind=REAL64)
    real(kind=REAL64), parameter :: startScalar = real(0.4d0,kind=REAL64)

    contains

        function get_wtime() result(t)
          use, intrinsic :: ISO_Fortran_env
          implicit none
          real(kind=REAL64) ::  t
          integer(kind=INT64) :: c, r
          call system_clock(count = c, count_rate = r)
          t = real(c,REAL64) / real(r,REAL64)
        end function get_wtime

        subroutine parseArguments()
#if defined(USE_DOCONCURRENT)
            use DoConcurrentStream, only: list_devices, set_device
#elif defined(USE_ARRAY)
            use ArrayStream, only: list_devices, set_device
#elif defined(USE_OPENACC)
            use OpenACCStream, only: list_devices, set_device
#elif defined(USE_OPENACCARRAY)
            use OpenACCArrayStream, only: list_devices, set_device
#else
            use SequentialStream, only: list_devices, set_device
#endif
            implicit none
            integer :: i, argc
            integer :: arglen,err,pos(2)
            character(len=64) :: argtmp
            argc = command_argument_count()
            do i=1,argc
                call get_command_argument(i,argtmp,arglen,err)
                if (err.eq.0) then
                    !
                    ! list devices
                    !
                    pos(1) = index(argtmp,"--list")
                    if (pos(1).gt.0) then
                        call list_devices()
                        stop
                    endif
                    !
                    ! set device number
                    !
                    pos(1) = index(argtmp,"--device")
                    if (pos(1).gt.0) then
                        if (i+1.gt.argc) then
                            print*,'You failed to provide a value for ',argtmp
                            stop
                        else
                            call get_command_argument(i+1,argtmp,arglen,err)
                            block
                                integer :: dev
                                read(argtmp,'(i15)') dev
                                call set_device(dev)
                            end block
                        endif
                    endif
                    !
                    ! array size
                    !
                    pos(1) = index(argtmp,"--arraysize")
                    pos(2) = index(argtmp,"-s")
                    if (any(pos(:).gt.0) ) then
                        if (i+1.gt.argc) then
                            print*,'You failed to provide a value for ',argtmp
                        else
                            call get_command_argument(i+1,argtmp,arglen,err)
                            read(argtmp,'(i15)') array_size
                        endif
                    endif
                    !
                    ! number of iterations
                    !
                    pos(1) = index(argtmp,"--numtimes")
                    pos(2) = index(argtmp,"-n")
                    if (any(pos(:).gt.0) ) then
                        if (i+1.gt.argc) then
                            print*,'You failed to provide a value for ',argtmp
                        else
                            call get_command_argument(i+1,argtmp,arglen,err)
                            read(argtmp,'(i15)') num_times
                            if (num_times.lt.2) then
                                write(*,'(a)') "Number of times must be 2 or more"
                                stop
                            end if
                        endif
                    endif
                    !
                    ! precision
                    !
                    pos(1) = index(argtmp,"--float")
                    if (pos(1).gt.0) then
                        write(*,'(a46,a39)') "Sorry, you have to recompile with -DUSE_FLOAT ", &
                                             "to run BabelStream in single precision."
                        stop
                    endif
                    !
                    ! selection (All, Triad, Nstream)
                    !
                    pos(1) = index(argtmp,"--triad-only")
                    if (pos(1).gt.0) then
                        selection = 2
                    endif
                    pos(1) = index(argtmp,"--nstream-only")
                    if (pos(1).gt.0) then
                        selection = 3
                    endif
                    !
                    ! CSV
                    !
                    pos(1) = index(argtmp,"--csv")
                    if (pos(1).gt.0) then
                        csv = .true.
                        write(*,'(a39)') "Sorry, CSV support isn't available yet."
                        stop
                    endif
                    !
                    ! units
                    !
                    pos(1) = index(argtmp,"--mibibytes")
                    if (pos(1).gt.0) then
                        mibibytes = .true.
                    endif
                    !
                    ! help
                    !
                    pos(1) = index(argtmp,"--help")
                    pos(2) = index(argtmp,"-h")
                    if (any(pos(:).gt.0) ) then
                        call get_command_argument(0,argtmp,arglen,err)
                        write(*,'(a7,a,a10)') "Usage: ", trim(argtmp), " [OPTIONS]"
                        write(*,'(a)') "Options:"
                        write(*,'(a)') "  -h  --help               Print the message"
                        write(*,'(a)') "  -s  --arraysize  SIZE    Use SIZE elements in the array"
                        write(*,'(a)') "  -n  --numtimes   NUM     Run the test NUM times (NUM >= 2)"
                        stop
                    endif
                end if
            end do
        end subroutine parseArguments

        subroutine run_all(timings, summ)
#if defined(USE_DOCONCURRENT)
            use DoConcurrentStream
#elif defined(USE_ARRAY)
            use ArrayStream
#elif defined(USE_OPENACC)
            use OpenACCStream
#elif defined(USE_OPENACCARRAY)
            use OpenACCArrayStream
#else
            use SequentialStream
#endif
            implicit none
            real(kind=REAL64), intent(inout) :: timings(:,:)
            real(kind=REAL64), intent(out) :: summ
            real(kind=REAL64) :: t1, t2
            integer(kind=StreamIntKind) :: i

            do i=1,num_times

                t1 = get_wtime()
                call copy()
                t2 = get_wtime()
                timings(1,i) = t2-t1

                t1 = get_wtime()
                call mul(startScalar)
                t2 = get_wtime()
                timings(2,i) = t2-t1

                t1 = get_wtime()
                call add()
                t2 = get_wtime()
                timings(3,i) = t2-t1

                t1 = get_wtime()
                call triad(startScalar)
                t2 = get_wtime()
                timings(4,i) = t2-t1

                t1 = get_wtime()
                summ = dot()
                t2 = get_wtime()
                timings(5,i) = t2-t1

            end do

        end subroutine run_all

        subroutine run_triad(timings)
#if defined(USE_DOCONCURRENT)
            use DoConcurrentStream
#elif defined(USE_ARRAY)
            use ArrayStream
#elif defined(USE_OPENACC)
            use OpenACCStream
#elif defined(USE_OPENACCARRAY)
            use OpenACCArrayStream
#else
            use SequentialStream
#endif
            implicit none
            real(kind=REAL64), intent(inout) :: timings(:,:)
            real(kind=REAL64) :: t1, t2
            integer(kind=StreamIntKind) :: i

            do i=1,num_times

                t1 = get_wtime()
                call triad(startScalar)
                t2 = get_wtime()
                timings(1,i) = t2-t1

            end do

        end subroutine run_triad

        subroutine run_nstream(timings)
#if defined(USE_DOCONCURRENT)
            use DoConcurrentStream
#elif defined(USE_ARRAY)
            use ArrayStream
#elif defined(USE_OPENACC)
            use OpenACCStream
#elif defined(USE_OPENACCARRAY)
            use OpenACCArrayStream
#else
            use SequentialStream
#endif
            implicit none
            real(kind=REAL64), intent(inout) :: timings(:,:)
            real(kind=REAL64) :: t1, t2
            integer(kind=StreamIntKind) :: i

            do i=1,num_times

                t1 = get_wtime()
                call nstream(startScalar)
                t2 = get_wtime()
                timings(1,i) = t2-t1

            end do

        end subroutine run_nstream

        subroutine check_solution(A, B, C, summ)
            implicit none
            real(kind=REAL64), intent(in) :: A(:), B(:), C(:)
            real(kind=REAL64), intent(in) :: summ

            integer(kind=StreamIntKind) :: i
            real(kind=REAL64) :: goldA, goldB, goldC, goldSum
            real(kind=REAL64) :: scalar

            ! always use double because of accumulation error
            real(kind=REAL64) :: errA, errB, errC, errSum, epsi

            goldA = startA
            goldB = startB
            goldC = startC
            goldSum = 0.0d0

            scalar = startScalar

            do i=1,num_times

                if (selection.eq.1) then
                    goldC = goldA
                    goldB = scalar * goldC
                    goldC = goldA + goldB
                    goldA = goldB + scalar * goldC
                else if (selection.eq.2) then
                    goldA = goldB + scalar * goldC
                else if (selection.eq.3) then
                    goldA = goldA + goldB + scalar * goldC;
                endif

            end do

            goldSum = goldA * goldB * array_size

            errA = SUM( ABS( A - goldA ) ) / array_size
            errB = SUM( ABS( B - goldB ) ) / array_size
            errC = SUM( ABS( C - goldC ) ) / array_size
            errSum = ABS( (summ - goldSum) /  goldSum)

            epsi = epsilon(real(0,kind=REAL64)) * 100.0d0

            if (errA .gt. epsi) then
                write(*,'(a38,e20.12)') "Validation failed on A. Average error ", errA
            end if
            if (errB .gt. epsi) then
                write(*,'(a38,e20.12)') "Validation failed on B. Average error ", errB
            end if
            if (errC .gt. epsi) then
                write(*,'(a38,e20.12)') "Validation failed on C. Average error ", errC
            end if

            if (selection.eq.1) then
                if (errSum .gt. 1.0e-8) then
                    write(*,'(a38,e20.12)') "Validation failed on Sum. Error ", errSum
                    write(*,'(a8,e20.12,a15,e20.12)') "Sum was ",summ, " but should be ", errSum
                end if
            endif

        end subroutine check_solution

end module BabelStreamUtil

program BabelStream
    use BabelStreamUtil
#if defined(USE_DOCONCURRENT)
    use DoConcurrentStream
#elif defined(USE_ARRAY)
    use ArrayStream
#elif defined(USE_OPENACC)
    use OpenACCStream
#elif defined(USE_OPENACCARRAY)
    use OpenACCArrayStream
#else
    use SequentialStream
#endif
    implicit none
    integer :: element_size, err
    real(kind=REAL64) :: scaling
    character(len=3) :: label
    real(kind=REAL64), allocatable :: timings(:,:)
    real(kind=REAL64), allocatable :: h_A(:), h_B(:), h_C(:)
    real(kind=REAL64) :: summ

    call parseArguments()

    write(*,'(a)')        "BabelStream Fortran"
    write(*,'(a9,f4.1)')  "Version: ", VERSION_STRING
    write(*,'(a16,a)')    "Implementation: ", implementation_name

    block
      character(len=32) :: printout
      write(printout,'(i9,1x,a5)') num_times,'times'
      write(*,'(a16,a)') 'Running kernels ',ADJUSTL(printout)
    end block
    write(*,'(a11,a6)') 'Precision: ',ADJUSTL(StreamRealName)

    element_size = storage_size(real(0,kind=REAL64)) / 8
    if (mibibytes) then
        scaling = 2.0d0**(-20)
        label   = "MiB"
    else
        scaling = 1.0d-6
        label   = "MB"
    endif

    write(*,'(a12,f9.1,a3)') 'Array size: ',1.0d0 * array_size * element_size * scaling, label
    write(*,'(a12,f9.1,a3)') 'Total size: ',3.0d0 * array_size * element_size * scaling, label

    allocate( timings(5,num_times) )

    call alloc(array_size)

    call init_arrays(startA, startB, startC)
    summ = 0.0d0

    timings = -1.0d0
    if (selection.eq.1) then
        call run_all(timings, summ)
    else if (selection.eq.2) then
        call run_triad(timings)
    else if (selection.eq.3) then
        call run_nstream(timings)
    endif

    allocate( h_A(1:array_size), h_B(1:array_size), h_C(1:array_size), stat=err)
    if (err .ne. 0) then
      write(*,'(a20,i3)') 'allocate returned ',err
      stop 1
    endif

    call read_arrays(h_A, h_B, h_C)
    call check_solution(h_A, h_B, h_C, summ)

    block
      character(len=12) :: printout(5)
      real(kind=REAL64) :: tmin,tmax,tavg,nbytes
      
      write(printout(1),'(a8)')   'Function'
      write(printout(2),'(a3,a8)') TRIM(label),'ytes/sec'
      write(printout(3),'(a9)')   'Min (sec)'
      write(printout(4),'(a3)')   'Max'
      write(printout(5),'(a7)')   'Average'
      write(*,'(5a12)') ADJUSTL(printout(1:5))
    
      if (selection.eq.1) then
        block
          integer, parameter :: sizes(5) = [2,2,3,3,2]
          character(len=5), parameter :: labels(5) = ["Copy ", "Mul  ", "Add  ", "Triad", "Dot  "]
          integer :: i
          do i=1,5
            tmin = MINVAL(timings(i,2:num_times))
            tmax = MAXVAL(timings(i,2:num_times))
            tavg = SUM(timings(i,2:num_times)) / num_times
            nbytes = element_size * array_size * sizes(i)
            write(printout(1),'(a12)')   labels(i)
            write(printout(2),'(f12.3)') scaling*nbytes/tmin
            write(printout(3),'(f12.5)') tmin
            write(printout(4),'(f12.5)') tmax
            write(printout(5),'(f12.5)') tavg
            write(*,'(5a12)') ADJUSTL(printout(1:5))
          enddo
        end block
      else if ((selection.eq.2).or.(selection.eq.3)) then
            tmin = MINVAL(timings(1,2:num_times))
            tmax = MAXVAL(timings(1,2:num_times))
            tavg = SUM(timings(1,2:num_times)) / num_times
            if (selection.eq.2) then
              nbytes = element_size * array_size * 3
              write(printout(1),'(a12)')   "Triad"
            else if (selection.eq.3) then
              nbytes = element_size * array_size * 4
              write(printout(1),'(a12)')   "Nstream"
            endif
            write(printout(2),'(f12.3)') scaling*nbytes/tmin
            write(printout(3),'(f12.5)') tmin
            write(printout(4),'(f12.5)') tmax
            write(printout(5),'(f12.5)') tavg
            write(*,'(5a12)') ADJUSTL(printout(1:5))
      endif
    end block

    call dealloc()

end program BabelStream
