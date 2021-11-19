#!/usr/bin/env fsharpi

open System
open System.IO
open System.Linq
open System.Threading
open System.Text

#r "System.Configuration"
open System.Configuration
#r "System.Net.Http.dll"

open System.Net.Http
open System.Net.Http.Headers

#r "System.Web.Extensions.dll"
open System.Web.Script.Serialization

#load "../InfraLib/Misc.fs"
#load "../InfraLib/Process.fs"
#load "../InfraLib/Git.fs"

open FSX.Infrastructure
open System.Collections
open System.Collections.Generic

let lastCommit = Git.GetLastCommit()
let currentBranch = Git.GetCurrentBranch()
let workflowCount = 1

let SendRequest (url: string) =
    async {
        use client = new HttpClient()
        client.DefaultRequestHeaders.Accept.Add(MediaTypeWithQualityHeaderValue.Parse("application/vnd.github.v3+json"))
        client.DefaultRequestHeaders.UserAgent.Add(ProductInfoHeaderValue("CIChecker", "1.0.0"))
        return! client.GetStringAsync(url) |> Async.AwaitTask
    }

let QueryRunCount (status: string) =
    async {
        let! response =
            sprintf
                "https://api.github.com/repos/nblockchain/geewallet/actions/runs?status=%s&branch=%s"
                status
                currentBranch
            |> SendRequest

        let responseObj =
            response
            |> JavaScriptSerializer().Deserialize<Dictionary<string, obj>>

        let runsArray =
            responseObj.["workflow_runs"] :?> ArrayList

        return
            runsArray.OfType<Dictionary<string, obj>>()
            |> Seq.filter (fun run -> run.["head_sha"] :?> string = lastCommit)
            |> Seq.length
    }

let rec WaitForAllRuns () =
    async {
        let! successfulCount = QueryRunCount("success")
        let! failedCount = QueryRunCount("failed")

        if failedCount > 0 then
            return false
        elif successfulCount < workflowCount then
            do! Async.Sleep(5000)
            return! WaitForAllRuns()
        else
            return true
    }

if WaitForAllRuns() |> Async.RunSynchronously then
    0
else
    -1
