Public Class Slot
    Public Property ore = ""
    Public Property hole = -1
    Public Property task = -1

    Public Sub New(ByVal o As String, ByVal h As Integer)
        ore = o
        hole = h
    End Sub

End Class


Public Class Entity

    Public Property uId As Integer = -1
    Public Property category As Integer = -1
    Public Property x As Integer = -1
    Public Property y As Integer = -1

    Public Sub New(ByVal _u, ByVal _c, ByVal _x, ByVal _y)
        uId = _u
        category = _c
        x = _x
        y = _y
    End Sub

End Class

Public Class Robo
    Inherits Entity

    Public Property verbose As Boolean = False
    Public Property inv As Integer = -1
    Public Property task As String = ""
    Public Property queue As Queue = New Queue()

    Public Sub New(ByVal _u, ByVal _c, ByVal _x, ByVal _y, ByVal _i)
        MyBase.New(_u, _c, _x, _y)
        inv = _i
    End Sub

    Public Sub gotOre()

        If verbose = True Then
            Console.Error.WriteLine("Robot #" & uId & " has ore, is overwriting orders and returning to base")
        End If

        ' override current task
        task = "MOVE 0 " & y

        ' clear queue
        clearQueue()

        ' issue command
        Console.WriteLine(task)
    End Sub

    Public Sub clearQueue()
        ' clear the queue 
        While queue.Count > 0
            queue.Dequeue()
        End While

    End Sub

    ' get next queued task or reset task if queue is empty
    Public Sub nextTask()
        If queue.Count = 0 Then
            task = ""
        Else
            task = queue.Dequeue()
        End If
    End Sub


    Public Sub action()
        ' if there is no current task, get a task from the queue and do it
        If task = "" Then
            If verbose = True Then
                Console.Error.WriteLine("Robot #" & uId & " has NO current order")
            End If

            nextTask()

            If task = "" Then
                If verbose = True Then
                    Console.Error.WriteLine("Robot #" & uId & " has NO queued orders")
                End If

                Console.WriteLine("WAIT")
            Else
                If verbose = True Then
                    Console.Error.WriteLine("Robot #" & uId & " grabs new order: " & task)
                End If

                Console.WriteLine(task)
            End If
            ' Bot has a current task
        Else
            If verbose = True Then
                Console.Error.WriteLine("Robot #" & uId & " has order: " & task)
            End If

            Dim command As String() = task.Split(" ")

            ' Robot has ore in inventory
            If inv = 4 Then
                gotOre()
                ' Robot has REQUEST command
            ElseIf command(0) = "REQUEST" Then
                ' If i have a radar
                If command(1) = "RADAR" And inv = 2 Then
                    task = queue.Dequeue()

                    If verbose = True Then
                        Console.Error.WriteLine("Robot #" & uId & " got RADAR and has new order: " & task)
                    End If

                    Console.WriteLine(task)

                    ' If i have a Trap
                ElseIf command(1) = "TRAP" And inv = 3 Then
                    task = queue.Dequeue()

                    If verbose = True Then
                        Console.Error.WriteLine("Robot #" & uId & " got TRAP and has new order: " & task)
                    End If

                    Console.WriteLine(task)

                    ' Keep trying to get Item
                Else
                    Console.WriteLine(task)
                End If
                ' Robot has MOVE command
            ElseIf command(0) = "MOVE" Then
                ' arrived at target grid
                If command(1) = x And command(2) = y Then
                    If verbose = True Then
                        Console.Error.WriteLine("Robot #" & uId & " has arrived at (" & x & " " & y & ")")
                    End If
                    nextTask()
                    If task = "" Then
                        If verbose = True Then
                            Console.Error.WriteLine("Robot #" & uId & " is waiting for new orders")
                        End If

                        Console.WriteLine("WAIT")
                    Else
                        If verbose = True Then
                            Console.Error.WriteLine("Robot #" & uId & " has new order: " & task)
                        End If

                        Console.WriteLine(task)
                    End If
                    ' else keep moving towards spot
                Else
                    Console.WriteLine(task)
                End If
                ' Robot has DIG command
            ElseIf command(0) = "DIG" Then
                ' check if im in bounds for digging
                If inv = 4 Then
                    gotOre()
                    ' if I am at my target
                ElseIf inv = -1 And ((x = command(1) And (y = command(2) - 1 Or y = command(2) + 1)) Or (y = command(2) And (x = command(1) - 1 Or x = command(1) + 1))) Then
                    Console.WriteLine(task)
                    nextTask()
                    ' else keep moving towards spot
                Else
                    Console.WriteLine(task)
                End If


                ' Robot has WAIT command
            ElseIf command(0) = "WAIT" Then
                If verbose = True Then
                    Console.Error.WriteLine("Robot #" & uId & " is waiting")
                End If

                Console.WriteLine(task)

                nextTask()
            End If
        End If

    End Sub

End Class

Module Player
    ' Deliver more ore to hq (left side of the map) than your opponent. Use radars to find ore but beware of traps!
    Public Function GetRand(ByVal Upper As Integer, ByVal Lower As Integer) As Integer
        ' by making Generator static, we preserve the same instance '
        ' (i.e., do not create new instances with the same seed over and over) '
        ' between calls '
        Static Generator As System.Random = New System.Random()
        Return Generator.Next(Lower, Upper)
    End Function

    Public Function dequeue(ByRef oreQueues() As Queue(Of String), ByVal width As Integer)
        For i As Integer = 0 To width
            If oreQueues(i).Count > 0 Then
                Return oreQueues(i).Dequeue()
            End If
        Next
        Return ""
    End Function


    Function nextRadar(ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal height As Integer) As IEnumerable
        Dim neighbors As New Queue(Of Integer())
        Dim targetX As Integer = 1
        Dim targetY As Integer = 0

        ' Right
        If x + 5 <= width - 1 Then
            targetX = x + 5
        Else
            targetX = width - 1
        End If

        ' Lower
        If y + 4 <= height - 1 Then
            targetY = y + 4
        Else
            targetY = height - 1
        End If
        neighbors.Enqueue({targetX, targetY})

        ' Upper       
        If y - 5 >= 0 Then
            targetY = y - 5
        Else
            targetY = 0
        End If
        neighbors.Enqueue({targetX - 1, targetY})


        Return neighbors
    End Function

    Sub Main()

        Dim verbose As Boolean = False

        ' Initialize Random Number Generator
        Randomize()

        ' size of the map
        Dim inputs As String() = Console.ReadLine().Split(" ")
        Dim width As Integer = inputs(0)
        Dim height As Integer = inputs(1)

        ' Data to persist for all turns
        Dim myBots As New Dictionary(Of Integer, Robo)
        Dim enemyBots As New Dictionary(Of Integer, Robo)

        Dim radars As New Dictionary(Of Integer, Entity)
        Dim radIdx As Integer = 0
        Dim radRdy As Boolean = True
        Dim fstRad As Boolean = True
        Dim radQueue As New Queue(Of String)
        Dim nexRadQueue As New Queue(Of Integer())
        Dim traps As New Dictionary(Of Integer, Entity)
        Dim trapIdx As Integer = 0

        Dim board(height - 1, width - 1) As Slot


        Dim oreQueues(width) As Queue(Of String)
        For i As Integer = 0 To width
            oreQueues(i) = New Queue(Of String)
        Next

        Dim oreCount As Integer = 0



        ' game loop
        While True
            Dim myScore As Integer ' Amount of ore delivered
            Dim opponentScore As Integer
            inputs = Console.ReadLine().Split(" ")
            myScore = inputs(0)
            opponentScore = inputs(1)

            For i As Integer = 0 To height - 1
                inputs = Console.ReadLine().Split(" ")
                For j As Integer = 0 To width - 1
                    Dim ore As String ' amount of ore or "?" if unknown
                    Dim hole As Integer ' 1 if cell has a hole
                    ore = inputs(2 * j)
                    hole = inputs(2 * j + 1)


                    If board(i, j) Is Nothing Then
                        Dim slot As New Slot(ore, hole)
                        board(i, j) = slot
                    Else
                        If ore <> "?" And board(i, j).task = -1 Then
                            board(i, j).task = Asc(ore) - Asc("0")
                        End If
                        board(i, j).ore = ore
                        board(i, j).hole = hole
                    End If


                    If board(i, j).ore <> "?" And board(i, j).task > 0 Then
                        Dim coords As String = j.ToString() & " " & i.ToString()
                        If oreQueues(j).Contains(coords) = False Then
                            oreQueues(j).Enqueue(coords)
                            oreCount += 1
                        End If
                    End If

                Next
            Next

            Dim entityCount As Integer ' number of entities visible to you
            Dim radarCooldown As Integer ' turns left until a new radar can be requested
            Dim trapCooldown As Integer ' turns left until a new trap can be requested


            inputs = Console.ReadLine().Split(" ")
            entityCount = inputs(0)
            radarCooldown = inputs(1)
            trapCooldown = inputs(2)
            If verbose = True Then
                Console.Error.WriteLine("Radar Cooldown: " & radarCooldown.ToString())
                Console.Error.WriteLine("Trap Cooldown: " & trapCooldown.ToString())
            End If

            ' reset boolean for bot getting a radar
            If radarCooldown > 0 Then
                radRdy = True
            End If

            For i As Integer = 0 To entityCount - 1
                Dim entityId As Integer ' unique id of the entity
                Dim entityType As Integer ' 0 for your robot, 1 for other robot, 2 for radar, 3 for trap
                Dim x As Integer ' x position of the entity
                Dim y As Integer ' y position of the entity
                Dim inv As Integer ' if this entity is a robot, the item it is carrying (-1 for NONE, 2 for RADAR, 3 for TRAP, 4 for ORE)

                inputs = Console.ReadLine().Split(" ")
                entityId = inputs(0)
                entityType = inputs(1)
                x = inputs(2)
                y = inputs(3)
                inv = inputs(4)
                ' My Robot
                If entityType = 0 Then
                    If myBots.ContainsKey(entityId) = True Then
                        ' Console.Error.WriteLine("Updating my Bot: " & entityId & ", X: " & x & ", Y: " & y)
                        myBots(entityId).x = x
                        myBots(entityId).y = y
                        myBots(entityId).inv = inv
                    Else
                        ' Console.Error.WriteLine("Adding my Bot: " & entityId & ", X: " & x & ", Y: " & y)
                        Dim bot As Robo = New Robo(entityId, entityType, x, y, inv)
                        myBots.Add(entityId, bot)
                    End If
                    ' Enemy Robot
                ElseIf entityType = 1 Then
                    If enemyBots.ContainsKey(entityId) = True Then
                        ' Console.Error.WriteLine("Updating enemy Bot: " & entityId & ", X: " & x & ", Y: " & y)
                        enemyBots(entityId).x = x
                        enemyBots(entityId).y = y
                        enemyBots(entityId).inv = inv
                    Else
                        ' Console.Error.WriteLine("Adding enemy Bot: " & entityId & ", X: " & x & ", Y: " & y)
                        Dim bot As Robo = New Robo(entityId, entityType, x, y, inv)
                        enemyBots.Add(entityId, bot)
                    End If
                    ' Radar
                ElseIf entityType = 2 Then
                    If radars.ContainsKey(entityId) = False Then
                        If verbose = True Then
                            Console.Error.WriteLine("Adding radar: " & entityId & ", X: " & x & ", Y: " & y)
                        End If
                        Dim sad As Entity = New Entity(entityId, entityType, x, y)
                        radars.Add(entityId, sad)
                    End If
                    ' Trap
                Else
                    If traps.ContainsKey(entityId) = True Then
                        If verbose = True Then
                            Console.Error.WriteLine("Updating trap: " & entityId & ", X: " & x & ", Y: " & y)
                        End If
                        traps(entityId).x = x
                        traps(entityId).y = y
                    Else
                        If verbose = True Then
                            Console.Error.WriteLine("Adding trap: " & entityId & ", X: " & x & ", Y: " & y)
                        End If
                        Dim trap As Entity = New Entity(entityId, entityType, x, y)
                        traps.Add(entityId, trap)
                    End If
                End If

            Next

            If verbose = True Then
                Dim cnt = 0
                For Each que As Queue(Of String) In oreQueues
                    If que.Count > 0 Then
                        For Each a As String In que
                            Dim cords = a.Split(" ")
                            Console.Error.WriteLine("Queue #" & cnt & " has (" & cords(0) & " " & cords(1) & ") @ quantity: " & board(cords(1), cords(0)).task)
                        Next
                    End If
                    cnt += 1
                Next
            End If

            ' Command Region 
            For Each bot As Robo In myBots.Values()

                Dim x As Integer = 0
                Dim y As Integer = 0

                If verbose = True Then
                    Console.Error.WriteLine("Ore Queue Count: " & oreCount)
                    Console.Error.WriteLine("Bot #" & bot.uId & " Inv: " & bot.inv & ", X: " & bot.x & ", Y: " & bot.y)
                End If

                ' Robot has no current or pending orders
                If bot.queue.Count = 0 And bot.task = "" Then

                    ' If robot is holding ore
                    If bot.inv = 4 Then
                        bot.gotOre()

                        ' if there is more ore than bots
                    ElseIf oreCount > 5 Then

                        ' order robot to dig up ore and then return it

                        ' get ore cords
                        Dim coords As String() = dequeue(oreQueues, width).split(" ")


                        x = coords(0)
                        y = coords(1)

                        ' lower ore count
                        oreCount -= 1
                        board(y, x).task -= 1

                        If verbose = True Then
                            Console.Error.WriteLine("Priority ordering Bot #" & bot.uId & " to DIG at " & x & " " & y)
                            Console.Error.WriteLine("Ore remaining in " & x & " " & y & ": " & board(y, x).task)
                        End If

                        ' issue command
                        bot.task = "DIG " & x.ToString() & " " & y.ToString()
                        bot.queue.Enqueue("MOVE 0 " & bot.y)
                        bot.action()
                        ' if there is a radar available
                    ElseIf radarCooldown = 0 And radRdy = True Then
                        ' order bot to get radar
                        If verbose = True Then
                            Console.Error.WriteLine("Ordering Bot #" & bot.uId & " to get RADAR")
                        End If

                        bot.task = "REQUEST RADAR"
                        radRdy = False

                        x = 0
                        y = 0

                        If fstRad = True Then
                            x = Int(width / 4)
                            y = Int(height / 2)

                            fstRad = False
                            If verbose = True Then
                                Console.Error.WriteLine("First Radar location (" & x & ", " & y & ")")
                            End If
                        Else
                            ' if there are no radars queued
                            If radQueue.Count = 0 Then
                                x = GetRand(Int(width * 3 / 4), Int(width * 1 / 4))
                                y = GetRand(Int(height * 3 / 4), Int(height * 1 / 4))
                                If verbose = True Then
                                    Console.Error.WriteLine("Random next Radar location from nextRadQueue (" & x & ", " & y & ")")
                                End If
                                ' if there are no more neighbors queued
                            ElseIf nexRadQueue.Count = 0 Then
                                Dim radCoords As String() = radQueue.Dequeue().Split(" ")
                                nexRadQueue = nextRadar(radCoords(0), radCoords(1), width, height)

                                Dim newCoords As Integer() = nexRadQueue.Dequeue()
                                x = newCoords(0)
                                y = newCoords(1)
                                If verbose = True Then
                                    Console.Error.WriteLine("Next Radar location from nextRadar (" & x & ", " & y & ")")
                                End If
                                ' grab next neighbor
                            Else
                                Dim coords As Integer() = nexRadQueue.Dequeue()

                                x = coords(0)
                                y = coords(1)
                                If verbose = True Then
                                    Console.Error.WriteLine("Next Radar location from nextRadQueue (" & x & ", " & y & ")")
                                End If
                            End If
                        End If

                        If verbose = True Then
                            Console.Error.WriteLine("Next Radar location (" & x & ", " & y & ")")
                        End If

                        radQueue.Enqueue(x & " " & y)

                        bot.queue.Enqueue("DIG " & x.ToString() & " " & y.ToString())
                        bot.queue.Enqueue("WAIT")

                        bot.action()
                        ' if ore is available
                    ElseIf oreCount > 0 Then

                        ' get ore cords
                        Dim coords As String() = dequeue(oreQueues, width).split(" ")


                        x = coords(0)
                        y = coords(1)

                        ' lower ore count
                        board(y, x).task -= 1
                        oreCount -= 1

                        ' order robot to dig up ore and then return it
                        If verbose = True Then
                            Console.Error.WriteLine("Ordering Bot #" & bot.uId & " to DIG at " & x & " " & y)
                            Console.Error.WriteLine("Ore remaining in " & x & " " & y & ": " & board(y, x).task)
                        End If

                        ' issue command
                        bot.task = "DIG " & x.ToString() & " " & y.ToString()
                        bot.queue.Enqueue("MOVE 0 " & bot.y)


                        bot.action()
                        ' if no ore and no radar then move to random nearby square
                    Else
                        If verbose = True Then
                            Console.Error.WriteLine("Ordering Bot #" & bot.uId & " to MOVE")
                        End If

                        bot.task = "MOVE " & GetRand(Int(width * 2 / 3), Int(width * 1 / 4)) & " " & GetRand(Int(height * 2 / 3), Int(height * 1 / 4))

                        bot.action()
                    End If
                    ' Robot has order and should continue    
                Else
                    If verbose = True Then
                        Console.Error.WriteLine("Robot #" & bot.uId & " has orders: " & bot.task)
                    End If
                    bot.action()
                End If
                If verbose = True Then
                    Console.Error.WriteLine()
                End If
            Next

        End While
    End Sub
End Module