void plot_sample_17hz(){
  TCanvas *c1 = new TCanvas("c1","Graph Draw Options",400,10,600,500);
  c1->Divide(1,2);
  c1->SetFillStyle(4100);
  c1->SetGrid();
  c1->GetFrame()->SetBorderSize(12);
  
  gStyle->SetOptStat(0);
  gStyle->SetLabelSize(0.07,"X");
  gStyle->SetLabelSize(0.07,"Y");
  gStyle->SetTitleOffset(0.7,"Y");
  
  float t1,t2,A,f,phi;
  
  TNtuple *nt = new TNtuple("nt","Demo ntuple","t1:t2:A:f:phi");
  ifstream data("LIGOtest.ana");
  int index=0;
  while (data >> t1 >> t2 >> A >> f >> phi) {
    nt->Fill(t1,t2,A,f,phi);
  }
  data.close();
  
  nt->SetMarkerStyle(7);
  c1->cd(1);
  nt->SetMarkerColor(1);
  nt->Draw("A:(t1+t2)/2>>htmp(20,0.0,10.0,20,0,1.0)","f<30","");
  TH2F *h = gPad->GetListOfPrimitives()->FindObject("htmp");
  h->SetTitle("");
  h->GetXaxis()->CenterTitle();
  h->GetYaxis()->CenterTitle();
  h->SetXTitle("Time [sec]");
  h->SetYTitle("Amplitude");
  gPad->ls();
  gPad->Modified();
  
  nt->SetMarkerColor(2);
  nt->Draw("A:(t1+t2)/2","f>10 && f<20","same");

  
  c1->cd(2); 
  nt->SetMarkerColor(1);
  nt->Draw("f:(t1+t2)/2>>htmp(20,0,10.0,20,0,30)","","");
  TH2F *h2 = gPad->GetListOfPrimitives()->FindObject("htmp");
  h2->SetTitle("");
  h2->GetXaxis()->CenterTitle();
  h2->GetYaxis()->CenterTitle();
  h2->SetXTitle("Time [sec]");
  h2->SetYTitle("Frequency [Hz]");
  gPad->ls();
  gPad->Modified();
  
  nt->SetMarkerColor(2);
  nt->Draw("f:(t1+t2)/2","f<30","same");
  
  c1->SaveAs("plot_sample_17hz.eps");
}
