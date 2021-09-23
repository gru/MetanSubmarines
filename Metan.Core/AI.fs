namespace Metan.Core

open System

type BotState =
    | SearchCrate
    | MoveTo of Position
    | Idle of Time

module AI =
    let private findMe game id =
        game.vehicles |> List.find (fun v -> v.id = id)
    
    let private findClosestCrate (cs:Crate list) hb =
        cs |> List.filter (fun c -> HitBox.intersect c.hitBox hb)
           |> List.tryHead
        
    let searchCrate (rnd:Random) (game:Game) id =
        let me = findMe game id
        let crate = me.hitBox
                    |> HitBox.expand 10
                    |> HitBox.crop game.size
                    |> findClosestCrate game.crates
        match crate with
        | Some c -> MoveTo(HitBox.topLeft c.hitBox), []
        | None -> MoveTo(Position.getRandom rnd game.size), [] 
        
    let move (game:Game) id pos =
        let me = findMe game id
        let tl = HitBox.topLeft me.hitBox
        let lx = (fst tl) - (fst pos)
        let ly = (snd tl) - (snd pos)
        if HitBox.intersect me.hitBox (HitBox.single pos) then
            Idle (game.time + 20u), []
        elif (abs lx) > (abs ly) then
            let dir =
                if lx > 0
                then Left
                else Right
            (MoveTo pos), [AreaCommand(Move(me.id, dir))]
        else
            let dir =
                if ly < 0
                then Up
                else Down
            (MoveTo pos), [AreaCommand(Move(me.id, dir))]
        
    let idle (game:Game) till =
        if game.time = till
        then SearchCrate, []
        else Idle till, []