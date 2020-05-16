/*
 * This is a simple program used to process the perceptual data collected
 * for the study of Music Similarity in Music Copyright Infringement. 
 * Please download org.json JAR files with all dependencies before running the 
 * program, and please specify the absolute path to the .jar file when compiling.
 */
package perceptualdataprocess;

import java.io.*;
import org.json.*;

/**
 *
 */
public class PerceptualDataProcess {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) throws Exception {

        // Read raw data of the perceptual data stored as JSON text
        String rawdata = null, s = null;
        File file = new File("/Users/username/PerceptualData/perceptual_raw20200309.txt");
        BufferedReader br = new BufferedReader(new FileReader(file));
        while ((s = br.readLine()) != null) {
            rawdata = s;
            System.out.println("Successfully read raw data stored as JSON text. ");
        } // while

        // Build JSON object for participants
        JSONArray participant_arr = new JSONArray(rawdata);
        int n = participant_arr.length(); // number of participants
        System.out.println("Number of participants: " + n + "\nUsernames: ");

        // IDs of 20 participants
        String[] usernames = new String[n];
        for (int i = 0; i < n; i++) {
            usernames[i] = participant_arr.getJSONObject(i).getString("username");
            System.out.println(usernames[i]);
        } // for

        // Build JSON object for answers of all participants
        JSONArray[] answer_arr = new JSONArray[n];
        for (int i = 0; i < n; i++) {
            answer_arr[i] = new JSONArray(participant_arr.getJSONObject(i).get("res").toString());
            System.out.println(usernames[i] + " answered " + answer_arr[i].length()
                    + " questions. "); // each participant answered 102 questions

            // Make sure all 102 questions were answered
            for (int j = 0; j < answer_arr[i].length(); j++) {
                if (answer_arr[i].getJSONObject(j).getBoolean("answered") == false) {
                    System.out.println("Question id=" + answer_arr[i].getJSONObject(j).getInt("id")
                            + " was not answered by " + usernames[i]
                            + ". Please remove this user and his/her answer from the raw data. ");
                    break;
                } // if
            } // for j
        } // for i

        Boolean[][] infringe_ans = new Boolean[n][102]; // the original answers of judgement on infringement or not
        int[][] similarity_ans = new int[n][102]; // the original answers of similarity
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < 102; j++) {
                int qid = answer_arr[i].getJSONObject(j).getInt("id");
                infringe_ans[i][qid] = answer_arr[i].getJSONObject(j).getBoolean("infringe");
                similarity_ans[i][qid] = answer_arr[i].getJSONObject(j).getInt("similarity");
            } // for j
        } // for i

        // Prepare for accuracy calculation
        Boolean[] court_decision = {true, false, true, false, false, false, false, true,
            true, true, false, false, true, true, false, true, true}; // court decisions of 17 cases
        Boolean[] infringe_court = new Boolean[102]; // court decisions of 102 questions
        for (int i = 0; i < 17; i++) {
            infringe_court[i] = court_decision[i];
        } // for
        for (int i = 0 + 17; i < 17 + 17; i++) {
            infringe_court[i] = court_decision[i - 17];
        } // for
        for (int i = 0 + 17 + 17; i < 17 + 17 + 17; i++) {
            infringe_court[i] = court_decision[i - 17 - 17];
        } // for
        for (int i = 0 + 17 + 17 + 17; i < 102; i++) {
            infringe_court[i] = false;
        } // for

        Boolean[][] infringe_TF = new Boolean[n][102]; // answers of judgement on infringement were correct or not
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < 102; j++) {
                if (infringe_ans[i][j] == infringe_court[j]) {
                    infringe_TF[i][j] = true;
                } else {
                    infringe_TF[i][j] = false;
                } // if
            } // for j
        } // for i

        // Calculate accuracy by participant
        double[][] accuracy_by_participant = new double[n][6];
        FileWriter accuracy_by_participant_csv = new FileWriter("accuracy_by_participant.csv");
        accuracy_by_participant_csv.append("Participant,full,melody,lyrics,full_random,melody_random,lyrics_random\n");
        for (int i = 0; i < n; i++) {
            accuracy_by_participant_csv.append(Integer.toString(i + 1) + ",");
            for (int j = 0; j < 6; j++) {
                int count = 0;

                if (j == 2) { // when the condition is lyrics-only
                    for (int k = 0 + 17 * j; k < 17 + 17 * j; k++) {
                        if ((k == 4 + 17 * j) || (k == 5 + 17 * j) || (k == 9 + 17 * j)) {
                            continue; // ignore 3 instrumental cases for lyrics-only analyse
                        } // if
                        if (infringe_TF[i][k] == true) {
                            count++;
                        } // if
                    } // for k
                    accuracy_by_participant[i][j] = (double) count / (17 - 3) * 100;
                } else {
                    for (int k = 0 + 17 * j; k < 17 + 17 * j; k++) {
                        if (infringe_TF[i][k] == true) {
                            count++;
                        } // if
                    } // for k
                    accuracy_by_participant[i][j] = (double) count / 17 * 100;
                } // if
                
                if (j == 5) {
                    accuracy_by_participant_csv.append(Double.toString(accuracy_by_participant[i][j]) + "\n");
                } else {
                    accuracy_by_participant_csv.append(Double.toString(accuracy_by_participant[i][j]) + ",");
                } // if
            } // for j
        } // for i
        accuracy_by_participant_csv.flush();
        accuracy_by_participant_csv.close();
        System.out.println("File accuracy_by_participant.csv was successfully generated. ");

        // Calculate accuracy by case
        double[][] accuracy_by_case = new double[17][3];
        FileWriter accuracy_by_case_csv = new FileWriter("accuracy_by_case.csv");
        accuracy_by_case_csv.append("Case,full,melody,lyrics\n");
        for (int i = 0; i < 17; i++) {
            accuracy_by_case_csv.append(Integer.toString(i) + ",");
            for (int j = 0; j < 3; j++) {
                // ignore 3 instrumental cases for lyrics-only analyse
                if (((j == 2) && (i == 4)) || ((j == 2) && (i == 5)) || ((j == 2) && (i == 9))) {
                    accuracy_by_case_csv.append("\n");
                    continue;
                } // if

                // normal cases
                int count = 0;
                for (int k = 0; k < n; k++) {
                    if (infringe_TF[k][i + 17 * j] == true) {
                        count++;
                    } // if
                } // for k
                accuracy_by_case[i][j] = (double) count / n * 100;
                if (j == 2) {
                    accuracy_by_case_csv.append(Double.toString(accuracy_by_case[i][j]) + "\n");
                } else {
                    accuracy_by_case_csv.append(Double.toString(accuracy_by_case[i][j]) + ",");
                } // if
            } // for j
        } // for i
        accuracy_by_case_csv.flush();
        accuracy_by_case_csv.close();
        System.out.println("File accuracy_by_case.csv was successfully generated. ");

        // Rename the original answers of similarity ratings from 0%-100% to 1-5
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < 102; j++) {
                switch (similarity_ans[i][j]) {
                    case 0:
                        similarity_ans[i][j] = 1;
                        break;
                    case 25:
                        similarity_ans[i][j] = 2;
                        break;
                    case 50:
                        similarity_ans[i][j] = 3;
                        break;
                    case 75:
                        similarity_ans[i][j] = 4;
                        break;
                    case 100:
                        similarity_ans[i][j] = 5;
                        break;
                    default:
                        System.out.println("Error found when renaming the original answers of similarity ratings. ");
                        break;
                } // switch
            } // for j
        } // for i
        
        // Calculate perceptual similarity
        double[][] perceptual_simi = new double[17][6];
        FileWriter perceptual_simi_csv = new FileWriter("perceptual_simi.csv");
        perceptual_simi_csv.append("Case,full_simi,melody_simi,lyrics_simi,full_random_simi,melody_random_simi,lyrics_random_simi\n");
        for (int i = 0; i < 17; i++) {
            perceptual_simi_csv.append(Integer.toString(i) + ",");
            for (int j = 0; j < 6; j++) {
                // ignore 3 instrumental cases for lyrics-only analyse
                if (((j == 2) && (i == 4)) || ((j == 2) && (i == 5)) || ((j == 2) && (i == 9))) {
                    perceptual_simi_csv.append(",");
                    continue;
                } // if

                // normal cases
                int sum = 0;
                for (int k = 0; k < n; k++) {
                    sum = sum + similarity_ans[k][i + 17 * j];
                } // for k
                perceptual_simi[i][j] = (double) sum / n;
                if (j == 5) {
                    perceptual_simi_csv.append(Double.toString(perceptual_simi[i][j]) + "\n");
                } else {
                    perceptual_simi_csv.append(Double.toString(perceptual_simi[i][j]) + ",");
                } // if
            } // for j
        } // for i
        perceptual_simi_csv.flush();
        perceptual_simi_csv.close();
        System.out.println("File perceptual_simi.csv was successfully generated. ");

        System.out.println("Perceptual data processing finished. ");
    } // main

} // class PerceptualDataProcess
