module modelos
    implicit none
    
    contains
    
    integer function doId (inputName, i) result (isDo)
    
        
        character (*), intent(in) :: inputName  ! nome do input para abrir
        integer, intent(inout) :: i             ! iterador do input
        
        integer :: input, stat                  ! variaveis controle p/ abrir os arq
        character(len=512) :: msg
        
        integer :: j                            ! iterador dos modelos
        character :: a                          ! caractere lido
        
        character :: d0(4), d0nt(7)             ! modelos que estamos procurando
        
        ! montagem dos modelos
        
        d0(1) = 'd'
        d0(2) = 'o'
        d0(3) = '('
        d0(4) = ')'
    
        d0nt(1) = 'd'
        d0nt(2) = 'o'
        d0nt(3) = 'n'
        d0nt(4) = "'"
        d0nt(5) = 't'
        d0nt(6) = '('
        d0nt(7) = ')'
        
        isDo = 0
        
        ! abrindo o input
        open(newunit=input, file= inputName, status="old", access="stream", action="read", iostat=stat, iomsg=msg)
        if (stat /= 0) then
            print *, trim(msg)
            error stop
        end if
        
        j = 1
        
        outer_loop: do while (.true.)
            ! le um caractere por vez
            read(input, POS=i, END = 100) a
            
            select case (j) 
                
                case (1)
                
                    ! se estiver de acordo, anda na file e no modelo
                    if (a == d0(j)) then
                        i = i + 1
                        j = j + 1
                    else
                    ! se nao sai
                        isDo = 0
                        exit
                    end if
                
                case (2)
                
                    if (a == d0(j)) then
                        i = i + 1
                        j = j + 1
                    else
                    ! se nao, sai
                        isDo = 0
                        exit
                    end if
                    
                case (3)
                    
                    if (a == d0(j)) then
                        i = i + 1
                        j = j + 1
                    else if (a == d0nt(j)) then
                        ! se o input(i) /= ( mas == '
                        ! troca a identificacao pra dont
                        i = i + 1
                        j = j + 1
                        
                        
                        do while (.true.)
                            read(input, POS=i, END = 100) a
                        
                            select case (j)
                                case (4:6)
                                    if (a == d0nt(j)) then
                                        i = i + 1
                                        j = j + 1
                                    else
                                    ! se nao sai
                                        isDo = 0
                                        exit outer_loop
                                    end if
                                case (7)
                                    if (a == d0nt(j)) then
                                        ! identifica como dont 
                                        print *, 'dont :('
                                        isDo = 1
                                        exit outer_loop
                                    end if
                                    ! se nao, sai
                                    isDo = 0
                                    exit outer_loop
                                case Default
                                    exit outer_loop
                                
                            end select
                        end do
                    
                    else
                    ! se nao, sai
                        isDo = 0
                        exit
                    end if
                
                case (4)
                    if (a == d0(j)) then
                        ! identifica como do
                        print *, 'do :)'
                        isDo = 2
                        exit
                    end if
                    
                    isDo = 0
                    exit
                
                case Default
                    exit
            
            end select
            
        end do outer_loop
        
        
        100 CONTINUE
        
        close(input)
        
    end function doId
    
    integer function multID (inputName, i) result (mult)
        
        character (*), intent(in) :: inputName  ! nome do input para abrir
        integer, intent(inout) :: i             ! iterador do input
        
        integer :: input, stat                  ! variaveis controle p/ abrir os arq
        character(len=512) :: msg
        integer :: ierror
        character(1) :: inte                    ! string para checar se é inteiro
        
        
        integer :: j                            ! iterador dos modelos
        character :: a                          ! caractere lido
        integer :: n, num                       ! numero lido, numero atual
        
        character :: mul(8)                     ! modelo que estamos procurando
        
        ! montagem do modelo
        
        mul(1) = 'm'
        mul(2) = 'u'
        mul(3) = 'l'
        mul(4) = '('
        mul(5) = 'n'
        mul(6) = ','
        mul(7) = 'n'
        mul(8) = ')'
        
        ! abrindo o input
        open(newunit=input, file= inputName, status="old", access="stream", action="read", iostat=stat, iomsg=msg)
        if (stat /= 0) then
            print *, trim(msg)
            error stop
        end if
        
        j = 1
        num = 0
        mult = 0
        do while (.true.)
            ! le um caractere por vez
            read(input, POS=i, END = 300) a
        
            select case (j) 
                
                case (1)
                
                    num = 0
                    ! se estiver de acordo, anda na file e no modelo
                    if (a == mul(j)) then
                        i = i + 1
                        j = j + 1
                    else
                    ! se nao, sai
                        mult = 0
                        exit
                    end if
                
                case (2 : 4, 6)
                
                    num = 0
                    if (a == mul(j)) then
                        i = i + 1
                        j = j + 1
                    else
                    ! se nao, sai
                        mult = 0
                        exit
                    end if
                    
                case (5, 7)
                    ! tenta "traduzir" para inteiro
                    read(input, POS=i) inte
                    read(inte, '(i1)', iostat = ierror) n
                    
                    if (ierror == 0) then
                        ! anda na file e soma o numero sendo construido
                        i = i + 1
                        num = 10 * num + n 
                    else
                        ! se caractere nao eh numero mas eh o proximo do modelo
                        ! aumenta o mult e anda no modelo
                        if (a == mul(6)) then
                            mult = num
                            j = j + 1
                            print *, num
                        else if (a == mul(8)) then
                            mult = mult * num
                            j = j + 1
                            print *, num
                        else
                        ! se nao, sai
                            mult = 0
                            exit
                        end if
                    end if
                    
                case (8)
                    if (a == mul(j)) then
                        ! anda na file, incrementa o mult total com o atual
                        print *, 'eba ', mult
                        exit
                    end if
                    
                    mult = 0
                    exit
                    
                case Default
                    exit
            
            end select
    
    end do
    
    300 CONTINUE
    
    close (input)
    
    end function multId

end module modelos
