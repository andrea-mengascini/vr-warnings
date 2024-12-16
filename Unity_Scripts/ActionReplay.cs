using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;

public class ActionReplay : MonoBehaviour
{
    public List<GameObject> objectsToRecord;
    private Vector3[] positions;
    private Quaternion[] rotations;

    public String fixedRecordingName;
    public String callableRecordingName;

    private List<ActionReplayRecord> actionReplayRecords = new List<ActionReplayRecord>();
    private bool isInReplayMode;
    private int currentReplayIndex;
    private string fixedFilePath;
    private string callableFilePath;
    public bool isRecording;
    public TextWriter fixed_tw;
    public TextWriter callable_tw;

    // Start is called before the first frame update
    void Start()
    {
        Debug.Log(Application.persistentDataPath);
        fixedFilePath = Path.Combine(Application.persistentDataPath, fixedRecordingName);
        fixed_tw = startRecordingFixed();
        isRecording = true;
        InvokeRepeating("RecordFixed", 1, (float)0.03);

        callableFilePath = Path.Combine(Application.persistentDataPath, callableRecordingName);
        callable_tw = startRecordingCallable();

        Debug.Log("fixedFilePath: " + fixedFilePath);
        Debug.Log("callableFilePath: " + callableFilePath);
    }


    private TextWriter startRecordingCallable()
    {
        if (!File.Exists(callableFilePath))
        {
            File.Create(callableFilePath).Dispose();
        }
        callable_tw = new StreamWriter(callableFilePath);
        return callable_tw;
    }
    private TextWriter startRecordingFixed()
    {
        if (!File.Exists(fixedFilePath))
        {
            File.Create(fixedFilePath).Dispose();
        }

        fixed_tw = new StreamWriter(fixedFilePath);

        string line = "";
        foreach (GameObject obj in objectsToRecord)
        {
            line += $"{obj.name}\n";
        }

        fixed_tw.WriteLine(line+"START");
        // Get the executable filename
        string appname = System.AppDomain.CurrentDomain.FriendlyName;
        fixed_tw.WriteLine(appname);

        positions = new Vector3[objectsToRecord.Count];
        rotations = new Quaternion[objectsToRecord.Count];
        line = "";
        //line = $"{Time.time}";
        int index = 0;

        foreach (GameObject obj in objectsToRecord)
        {
            positions[index] = obj.transform.position;
            rotations[index] = obj.transform.rotation;
            index++;
            line += $"{obj.name}\n({obj.transform.position.x},{obj.transform.position.y},{obj.transform.position.z})\n({obj.transform.rotation.x},{obj.transform.rotation.y},{obj.transform.rotation.z},{obj.transform.rotation.w})\n";
        }
        line += "INITIAL";
        fixed_tw.WriteLine(line);

        return fixed_tw;
    }

    public void finishRecording()
    {
        isRecording = false;
        fixed_tw.WriteLine("END");
        fixed_tw.Close();
        callable_tw.WriteLine("END");
        callable_tw.Close();
    }


    private void SetTransform(int index)
    {
        currentReplayIndex = index;

        ActionReplayRecord actionReplayRecord = actionReplayRecords[index];

        transform.position = actionReplayRecord.position;
        transform.rotation = actionReplayRecord.rotation;
    }

    private void RecordFixed()
    {
        if (isRecording)
        {
            String line = $"{Time.time}\n";

            int index = 0;
            foreach(GameObject obj in objectsToRecord)
            {
                if (positions[index] != obj.transform.position || rotations[index] != obj.transform.rotation)
                {
                    positions[index] = obj.transform.position;
                    rotations[index] = obj.transform.rotation;
                    line += $"{obj.name}\n({obj.transform.position.x},{obj.transform.position.y},{obj.transform.position.z})\n({obj.transform.rotation.x},{obj.transform.rotation.y},{obj.transform.rotation.z},{obj.transform.rotation.w})\n";
                }
                index++;

            }
            if (line != "")
            {
                line += $"STOP";
                fixed_tw.WriteLine(line);
            }
            //fixed_tw.WriteLine(line);
        }
        else
        {
            CancelInvoke("Record");
            //finishRecording();
        }

    }

    public void writeCallable(String line)
    {
        line += $" {Time.time}";
        callable_tw.WriteLine(line);
    }


}