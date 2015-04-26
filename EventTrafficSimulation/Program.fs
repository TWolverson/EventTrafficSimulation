// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System

type msg = 
        | CreateAgent
        | DoStuff of string
        | DoStuffResponse of string

[<EntryPoint>]
let main argv = 
    let pool = MailboxProcessor.Start(fun master ->
        let rec messageloop agents = async {
            let! msg = master.Receive()
            match msg with
            | CreateAgent ->
                printfn "Received CreateAgent"
                let newagent = MailboxProcessor.Start(fun agent ->
                    let rec forever = async {
                        let! ggg = agent.Scan(fun m -> 
                            let condition t = 
                                t = "hello"
                            match m with
                            | DoStuff t when condition t -> 
                                Some( async { () }) 
                            | _ -> None);
                        let! mymsg = agent.Receive();
                        match mymsg with
                        | DoStuff t ->
                            printfn "Responding to DoStuff"
                            master.Post(DoStuffResponse "response")
                            |> ignore
                        | _ -> ()
                        return! forever
                    }
                    forever
                )
                return! messageloop(newagent :: agents)
            | DoStuffResponse t ->
                printfn "Received response"
                return! messageloop(agents)
            | _ -> 
                printfn "Relaying message"
                for agent in agents do agent.Post msg
                return! messageloop(agents)
        }
        messageloop []
    )

    pool.Post(CreateAgent)
    pool.Post(CreateAgent)
    pool.Post(DoStuff "hello")
                  
    System.Console.ReadKey() |> ignore
    printfn "%A" argv
    0 // return an integer exit code