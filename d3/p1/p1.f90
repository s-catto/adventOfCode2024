program p1

    implicit none
    
    integer :: input, stat                  ! variaveis controle p/ abrir os arq
    character(len=512) :: msg
    integer :: ierror
    
    character :: a                          ! caractere a ser lido do input
    character(1) :: inte                    ! string para checar se é inteiro
    character :: modelo(8)                  ! modelo que estamos procurando
    integer :: i , j                        ! iteradores
    
    integer :: n, num,  mult                ! numero lido, numero atual, mult atual
    integer :: multTudo                     ! soma total dos mults (resposta)
    
    ! abrindo o input
    open(newunit=input, file="../input", status="old", access="stream", action="read", iostat=stat, iomsg=msg)
    if (stat /= 0) then
        print *, trim(msg)
        error stop
    end if
    
    ! construção do modelo que estamos procurando
    modelo(1) = 'm'
    modelo(2) = 'u'
    modelo(3) = 'l'
    modelo(4) = '('
    modelo(5) = 'n'
    modelo(6) = ','
    modelo(7) = 'n'
    modelo(8) = ')'
    
    i = 1
    j = 1
    num = 0
    mult = 1
    multTudo = 0
    
    do while (.true.)
        ! le um caractere por vez
        read(input, POS=i, END = 200) a
        
        select case (j) 
            
            case (1)
            
                num = 0
                ! se estiver de acordo, anda na file e no modelo
                if (a == modelo(j)) then
                    i = i + 1
                    j = j + 1
                else
                ! se nao so na file
                    i = i + 1
                    mult = 1
                end if
            
            case (2 : 4, 6)
            
                num = 0
                if (a == modelo(j)) then
                    i = i + 1
                    j = j + 1
                else
                ! se nao, volta pra testar se eh modelo(1)
                    j = 1
                    mult = 1
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
                    if (a == modelo(6)) then
                        mult = num
                        j = j + 1
                        print *, num
                    else if (a == modelo(8)) then
                        mult = mult * num
                        j = j + 1
                        print *, num
                    else
                    ! se nao, volta pra testar se eh modelo(1)
                        j = 1
                    end if
                end if
                
            case (8)
                if (a == modelo(j)) then
                    ! anda na file, incrementa o mult total com o atual
                    i = i + 1
                    multTudo = multTudo + mult
                    print *, 'eba ', mult
                end if
                
                ! volta pra 1
                j = 1
                mult = 1
            case Default
        
        end select
    
    end do
    
    200 CONTINUE
    
    print *, multTudo
    
    close(input)
    
end program p1
